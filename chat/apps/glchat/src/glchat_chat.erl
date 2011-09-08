%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%% Chat!
%%% @end
%%% Created : 17 May 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_chat).

-behaviour(gen_server).

-include("glchat.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  uuid, 
  messages,
  participants,
  sequence,
  hungry,
  title,
  url
}).

% Config
-define(IDLE_CHECK_TIME, 10000).

% Initial message buffer size
-define(BUFFER_SIZE, 50).


% Module shorthands
-define(PARTS, glchat_participants).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Code) ->
    gen_server:start_link(?MODULE, [Code], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Code]) ->
    case glchat_mongo:get_chat(Code) of
        {ok, Info} ->
            gproc:reg({n, l, {chat, Code}}),

            {ok, Conf} = application:get_env(chat),
            put(trace, ?GV(trace, Conf)),

            UUID = ?GV(<<"uuid">>, Info),
            put(chat_uuid, UUID),

            Seq = case ?GV(<<"last_message_sequence">>, Info) of
                      undefined -> 0;
                      Val -> round(Val)
                  end,
                                   
            Participants = ?GVD(<<"participants">>, Info, []),
            {array, Messages} = ?GVD(<<"messages">>, Info, {array, []}),

            Title = case ?GV(<<"title">>, Info) of
                        undefined -> <<"Update: ", UUID/binary>>;
                        "" -> <<"Update: ", UUID/binary>>;
                        TitleStr -> TitleStr
                    end,

            URL = case ?GV(<<"url">>, Info) of
                      undefined -> undefined;
                      "" -> undefined;
                      URLStr -> URLStr
                  end,

            State0 = #state{
              uuid = UUID,
              messages = lists:reverse(Messages),
              participants = ?PARTS:new(Participants),
              hungry = ?GVD(<<"hungry">>, Info, false),
              title = Title,
              url = URL,
              sequence = Seq
            },

            case State0#state.hungry == true
                andalso length(?PARTS:hungry_list(State0#state.participants)) < 2 of
                true -> 
                    ?DBG({chat_finished, State0#state.uuid}),
                    ?PARTS:commit(State0#state.participants, [{<<"finished">>, {set, true}}]),
                    {stop, normal};
                _ -> 
                    timer:send_interval(?IDLE_CHECK_TIME, idle_check),
                    IdleTime = ?GV(idle_time, Conf),
                    timer:send_interval(IdleTime * 1000, dispatch_idle),
                    State = dispatch_idle(idle_check(State0)),
                    ?DBG({chat_started, Code}),
                    {ok, State}

            end;

        {error, Error} ->
            ?DBG({chat_start_failed, Code, Error}),
            {stop, Error}
    end.



handle_call({join, ParticipantUUID}, _From, State) ->
    case glchat_mongo:get_chat(State#state.uuid, ParticipantUUID) of
        {ok, Participant} ->
            NewParts0 = ?PARTS:ensure(Participant, State#state.participants),
            NewParts = ?PARTS:commit(?PARTS:seq(ParticipantUUID, State#state.sequence, NewParts0)),
            NewState = State#state{participants=NewParts},
            Reply = [{userUUID, ?PARTS:get_field(ParticipantUUID, <<"session_uuid">>, NewParts)},
                     {chat, NewState#state.uuid},
                     {participants, ?PARTS:public_list(NewParts)},
                     {messages, [list_to_tuple(munge_message(M, NewParts)) 
                                 || M <- lists:sublist(NewState#state.messages, ?BUFFER_SIZE)]}];
        {error, _Error} ->
            NewState = State,
            Reply = none
    end,
    {reply, Reply, NewState};

handle_call({subscribe, ParticipantUUID}, _From, State) ->
    participant_call(ParticipantUUID, State, fun handle_seq/2);

handle_call({leave, ParticipantUUID}, _From, State) ->
    participant_call(ParticipantUUID, State, fun handle_leave/2);

handle_call({message, ParticipantUUID, Content}, _From, State) ->
    handle_call({message, ParticipantUUID, Content, true}, _From, State);
handle_call({message, ParticipantUUID, Content, Touch}, _From, State) ->
    participant_call(ParticipantUUID, State, 
                     fun(PartPL, _S) -> 
                             handle_message(PartPL, ParticipantUUID, Content, Touch, State)
                     end);

handle_call({broadcast, Message0}, _From, State) ->
    Seq = State#state.sequence + 1,
    UUID = State#state.uuid,
    Message = [{<<"type">>, <<"broadcast">>},
               {<<"created">>, glchat_util:unix_timestamp()},
               {<<"sequence">>, Seq}
               | Message0],

    MessageChanges = [{<<"messages">>, {push, Message}},
                      {<<"last_message_sequence">>, {set, Seq}}],
    NewParts = ?PARTS:commit(State#state.participants, MessageChanges),
    gproc:send({p, l, UUID}, {push, {message, UUID, Seq}, munge_message(Message, NewParts)}),

    NewState = State#state{
                 sequence=Seq,
                 participants=NewParts,
                 messages=[Message|State#state.messages]},

    {reply, {ok, Seq}, NewState};

handle_call({history, Start, Stop, ParticipantUUID}, _From, State) ->
    participant_call(ParticipantUUID, State, 
                     fun(_, _) -> handle_history(Start, Stop, State) end);


handle_call(partlist, _From, State) ->
    PartList = ?PARTS:hungry_list(State#state.participants),
    {reply, PartList, State};

handle_call(useridlist, _From, State) ->
    UIDList = ?PARTS:userid_list(State#state.participants),
    {reply, UIDList, State};

handle_call(Request, _From, State) ->
    ?DBG({unexpected_call, Request}),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    ?DBG({unexpected_cast, Msg}),
    {noreply, State}.


handle_info({seq, ParticipantUUID, Seq}, State) ->
    NewParts = ?PARTS:seq(ParticipantUUID, Seq, State#state.participants),
    {noreply, State#state{participants=NewParts}};

handle_info({connection_died, ParticipantUUID}, State) ->
    NewState = case gproc:lookup_pids({p, l, {participant_session, ParticipantUUID}}) of
                   [] ->
                       NewParts = ?PARTS:commit(?PARTS:untouch(ParticipantUUID, State#state.participants)),
                       State#state{participants=NewParts};
                   _Pids ->
                       State
               end,
    {noreply, NewState};

handle_info(idle_check, State) ->
    {noreply, idle_check(State)};

handle_info(dispatch_idle, State) ->
    {noreply, dispatch_idle(State)};

handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.


terminate(_Reason, State) ->
    ?PARTS:commit(State#state.participants),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%%%===================================================================
%%% Participant-only method handlers
%%%===================================================================

participant_call(ParticipantUUID, State, Handler) ->
    case ?PARTS:find(ParticipantUUID, State#state.participants) of
        error -> 
            {reply, {error, not_participant}, State};
        {ok, PartPL} -> 
            Handler(PartPL, State)
    end.


handle_leave(PartPL, State) ->
    NewParts = ?PARTS:commit(?PARTS:remove(PartPL, State#state.participants)),
    
    NewState = State#state{participants=NewParts},

    case NewState#state.hungry == true
        andalso length(?PARTS:hungry_list(NewParts)) < 2 of
        true -> 
            ?DBG({chat_finished, State#state.uuid}),
            ?PARTS:commit(NewParts, [{<<"finished">>, {set, true}}]),
            {stop, normal, false, NewState};
        _ -> 
            {reply, true, NewState}
    end.

handle_seq(PartPL, State) ->
    UserSeq = case dict:find(<<"seen_seq">>, PartPL) of
                  {ok, S} -> S;
                  error -> 0
              end,
    Diff = State#state.sequence - UserSeq,
    {reply, Diff, State}.


handle_message(_PartPL, ParticipantUUID, Content, Touch, State) ->
    Seq = State#state.sequence + 1,

    Message = [{<<"sender">>, ParticipantUUID},
               {<<"created">>, glchat_util:unix_timestamp()},
               {<<"content">>, Content},
               {<<"sequence">>, Seq}
               ],

    UUID = State#state.uuid,

    NewParts0 = case Touch of
                    true -> ?PARTS:touch(ParticipantUUID, State#state.participants);
                    _ -> State#state.participants
                end,
    
    Title = case State#state.hungry of
                true -> none;
                false -> State#state.title
            end,

    NewParts1 = ?PARTS:pushnotify(ParticipantUUID, Content, Title, NewParts0),

    MessageChanges = [{<<"messages">>, {push, Message}},
                      {<<"last_message_sequence">>, {set, Seq}}],

    NewParts = ?PARTS:commit(NewParts1, MessageChanges),
    gproc:send({p, l, UUID}, {push, {message, UUID, Seq}, munge_message(Message, NewParts)}),

    NewState = State#state{
                 sequence=Seq,
                 participants=NewParts,
                 messages=[Message|State#state.messages]},

    {reply, {ok, Seq}, NewState}.


handle_history(Start, Stop, State) ->
    Prefix = lists:dropwhile(fun(M) -> ?GV(<<"sequence">>, M) > Stop end, State#state.messages),
    History = lists:takewhile(fun(M) -> ?GV(<<"sequence">>, M) >= Start end, Prefix),
    Munged = lists:reverse([list_to_tuple(munge_message(M, State#state.participants)) || M <- History]),
    {reply, Munged, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

idle_check(State) ->
    {ok, Conf} = application:get_env(chat),
    InactiveTime = ?GV(inactive_time, Conf),
    NewParts = ?PARTS:set_unseen_to_inactive(InactiveTime, State#state.participants),
    State#state{participants=NewParts}.


dispatch_idle(#state{hungry=true}=State) ->
    State;
dispatch_idle(State) ->
    {ok, Conf} = application:get_env(chat),
    IdleTime = ?GV(idle_time, Conf),
    NewParts = ?PARTS:dispatch_idle(IdleTime, State#state.messages, 
                                    State#state.title, 
                                    [{url, State#state.url},
                                     {title, State#state.title}],
                                    State#state.participants),
    State#state{participants=?PARTS:commit(NewParts)}.
    
munge_message(Message, Parts) ->
    case ?GV(<<"type">>, Message) of
        <<"broadcast">> ->
            Message;
        _ ->
            SenderUUID = ?GV(<<"sender">>, Message),
            SessionUUID = glchat_participants:get_field(SenderUUID, <<"session_uuid">>, Parts),
            [{<<"type">>, <<"message">>},{<<"sender_uuid">>, SessionUUID}|
             [{K,V} || {K, V} <- Message, K /= <<"sender">>]]
    end.
