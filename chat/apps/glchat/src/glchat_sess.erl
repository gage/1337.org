%%%-------------------------------------------------------------------
%%% @author  <gulu@dev3.cogini.com>
%%% @copyright (C) 2011,
%%% @doc
%%% Generic comet session behaviour
%%% @end
%%% Created : 11 May 2011 by  <gulu@dev3.cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_sess).

-behaviour(gen_server).

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([spawn_session/1, find/1, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("glchat.hrl").


-define(SERVER, ?MODULE).

-define(GPROCKEY(UUID), {n,l,{session,UUID}}).

-record(state, {
  key,
  participantUUID,
  userID = none,
  chatUUIDs = [],
  seqs = gb_trees:empty(),
  sentSeqs = gb_trees:empty(),
  lastAck = 0,

  lastPollTime,

  listener = none,
  queue = [],

  firstPoll = true
}).


%%%===================================================================
%%% Behaviour
%%%===================================================================

behaviour_info(callbacks) ->
    [{handle, 4}];
behaviour_info(_Other) ->
    undefined.


%%%===================================================================
%%% API
%%%===================================================================

spawn_session(ParticipantUUID) ->
    UUID = list_to_binary(glchat_util:format_uuid(glchat_util:random_uuid())),
    case gproc:lookup_pids(?GPROCKEY(UUID)) of
        [_] ->
            spawn_session(ParticipantUUID);
        [] ->
            {ok, Pid} = supervisor:start_child(glchat_sess_sup, [ParticipantUUID, UUID]),
            {ok, UUID, Pid}
    end.

find(UUID) ->
    gproc:where(?GPROCKEY(UUID)).

start_link(ParticipantUUID, Key) ->
    gen_server:start_link(?MODULE, [ParticipantUUID, Key], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ParticipantUUID, Key]) ->
    gproc:reg(?GPROCKEY(Key), self()),
    gproc:reg({p, l, {participant_session, ParticipantUUID}}),
    UserID = case glchat_mongo:get_user_id(ParticipantUUID) of
                 none -> 
                     none;
                 UID ->
                     gproc:reg({p, l, {user_session, UID}}),
                     UID
             end,

    %?DBG({session_running, ParticipantUUID, UserID, Key}),
    {ok, Conf} = application:get_env(http),
    Timeout = ?GV(session_timeout, Conf),
    timer:send_interval(Timeout, defunctCheck),

    {ok, #state{
       key = Key,
       participantUUID = ParticipantUUID,
       userID = UserID,
       lastPollTime = now()
      }}.

handle_call({poll, Ack}, From, #state{lastAck=Ack}=State) ->
    handle_poll(From, confirm_seqs(State));

handle_call({poll, _}, From, State) ->
    handle_poll(From, State);


handle_call({call, join, [ChatUUIDs]}, _From, State) ->
    keepalive(State),
    Infos = glchat_sess_api:handle(join, [ChatUUIDs], State#state.participantUUID, State#state.userID),
    ValidChatUUIDs = [UUID || UUID <- [?GV(chat, Info) || Info <- Infos], UUID /= undefined],
    {reply, [list_to_tuple(I) || I <- Infos], State#state{chatUUIDs=ValidChatUUIDs}};

handle_call({call, Method, Args}, _From, State) ->
    keepalive(State),
    Reply = glchat_sess_api:handle(Method, Args, State#state.participantUUID, State#state.userID),
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    ?DBG({unexpected_call, Request}),
    {reply, ok, State}.



handle_cast(Msg, State) ->
    ?DBG({unexpected_cast, Msg}),
    {noreply, State}.


handle_info({push, {message, UUID, Seq}, Obj}, State) ->
    NewSeqs = case lists:member(UUID, State#state.chatUUIDs) of
                  true -> gb_trees:enter(UUID, Seq, State#state.seqs);
                  _ -> State#state.seqs
              end,
    handle_info({push, UUID, Obj}, State#state{seqs=NewSeqs});

handle_info({push, Key, Obj}, State) when is_atom(Key) ->
    handle_info({push, list_to_binary(atom_to_list(Key)), Obj}, State);

handle_info({push, Key, Obj}, #state{listener=none}=State) ->
    Msg = [{key, Key}|Obj],
    NewQueue = [Msg|State#state.queue],
    {noreply, State#state{queue=NewQueue}};

handle_info({push, Key, Obj}, #state{listener=Listener}=State) ->
    Msg = [{key, Key}|Obj],
    gen_server:reply(Listener, {State#state.lastAck, [Msg]}),
    {noreply, send_seqs(State#state{listener=none})};

handle_info({join_chat, ChatUUID}, #state{participantUUID=ParticipantUUID, userID=UserID}=State) ->
    case glchat_sess_api:handle(join, [[ChatUUID]], ParticipantUUID, UserID) of
        [Info] ->
            Msg = [{<<"type">>, <<"joinchat">>},
                   {<<"data">>, list_to_tuple(Info)}],
            self() ! {push, <<"hungry">>, Msg};
        Other ->
            ?DBG({implicit_join_failed, ChatUUID, UserID, Other})
    end,
    {noreply, State};

handle_info({poll_timeout, PollID}, #state{lastPollTime=PollID}=State) ->
    case State#state.listener of
        none -> ok;
        _Listener -> gen_server:reply(State#state.listener, {State#state.lastAck, []})
    end,
    {noreply, State#state{listener=none}};

handle_info({poll_timeout, _OldPollID}, State) ->
    {noreply, State};



handle_info(defunctCheck, State) ->
    Diff = timer:now_diff(now(), State#state.lastPollTime),
    {ok, Conf} = application:get_env(http),
    Timeout = ?GV(session_timeout, Conf) * 1000,
    case Diff > Timeout of
        true ->
            [(catch gproc:send({n, l, {chat, C}}, {connection_died, State#state.participantUUID}))
             || C <- State#state.chatUUIDs],
            {stop, normal, State};
        false ->
            {noreply, State}
    end;


handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_poll(From, #state{listener=none, queue=[]}=State) ->
    Timeout = case State#state.firstPoll of
                  true -> 2000;
                  _ ->
                      {ok, Conf} = application:get_env(http),
                      ?GV(poll_timeout, Conf)
              end,
    PollID = now(),
    timer:send_after(Timeout, {poll_timeout, PollID}),
    {noreply, State#state{listener=From, lastPollTime=PollID, firstPoll=false, 
                          lastAck=State#state.lastAck + 1}};

handle_poll(_From, #state{listener=none, queue=[_|_]}=State) ->
    Msgs = lists:reverse(State#state.queue),
    Ack = State#state.lastAck + 1,
    {reply, {Ack, Msgs}, send_seqs(State#state{queue=[], lastPollTime=now(), lastAck=Ack})};

handle_poll(From, #state{listener=OldFrom}=State) ->
    gen_server:reply(OldFrom,[]),
    handle_poll(From, State#state{listener=none}).


keepalive(#state{userID=none}) -> 
    ok;
keepalive(#state{userID=UserID}) ->
    gproc:send({p, l, {user_cache, UserID}}, keepalive).


send_seqs(#state{seqs=Seqs, sentSeqs=SentSeqs}=State) ->
    NewSent = merge_seqs(gb_trees:iterator(Seqs), SentSeqs),
    State#state{seqs = gb_trees:empty(),
                sentSeqs = NewSent}.

confirm_seqs(#state{sentSeqs=Seqs}=State) ->
    ParticipantUUID = State#state.participantUUID,
    [gproc:send({n, l, {chat, UUID}}, {seq, ParticipantUUID, Seq})
     || {UUID, Seq} <- gb_trees:to_list(Seqs)],
    State#state{sentSeqs=gb_trees:empty()}.
        

merge_seqs(Iter, SentSeqs) ->
    case gb_trees:next(Iter) of
        none ->
            SentSeqs;
        {Key, Val, Iter2} ->
            NewSent = case gb_trees:lookup(Key, SentSeqs) of
                          {value, OldVal} when OldVal < Val ->
                              gb_trees:enter(Key, Val, SentSeqs);
                          none ->
                              gb_trees:enter(Key, Val, SentSeqs);
                          _ ->
                              SentSeqs
                      end,
            merge_seqs(Iter2, NewSent)
    end.