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

init([_Code]) ->
    ?DBG({chat_start_failed}),
    {stop, error}.
    
    % ==== Implement this
    
    % case glchat_mongo:get_chat(Code) of
    %     {ok, Info} ->
    %         gproc:reg({n, l, {chat, Code}}),
    % 
    %         {ok, Conf} = application:get_env(chat),
    %         put(trace, ?GV(trace, Conf)),
    % 
    %         UUID = ?GV(<<"uuid">>, Info),
    %         put(chat_uuid, UUID),
    % 
    %         Seq = case ?GV(<<"last_message_sequence">>, Info) of
    %                   undefined -> 0;
    %                   Val -> round(Val)
    %               end,
    %                                
    %         Participants = ?GVD(<<"participants">>, Info, []),
    %         {array, Messages} = ?GVD(<<"messages">>, Info, {array, []}),
    % 
    %         Title = case ?GV(<<"title">>, Info) of
    %                     undefined -> <<"Update: ", UUID/binary>>;
    %                     "" -> <<"Update: ", UUID/binary>>;
    %                     TitleStr -> TitleStr
    %                 end,
    % 
    %         URL = case ?GV(<<"url">>, Info) of
    %                   undefined -> undefined;
    %                   "" -> undefined;
    %                   URLStr -> URLStr
    %               end,
    % 
    %         State0 = #state{
    %           uuid = UUID,
    %           messages = lists:reverse(Messages),
    %           participants = ?PARTS:new(Participants),
    %           hungry = ?GVD(<<"hungry">>, Info, false),
    %           title = Title,
    %           url = URL,
    %           sequence = Seq
    %         },
    % 
    %         case State0#state.hungry == true
    %             andalso length(?PARTS:hungry_list(State0#state.participants)) < 2 of
    %             true -> 
    %                 ?DBG({chat_finished, State0#state.uuid}),
    %                 ?PARTS:commit(State0#state.participants, [{<<"finished">>, {set, true}}]),
    %                 {stop, normal};
    %             _ -> 
    %                 timer:send_interval(?IDLE_CHECK_TIME, idle_check),
    %                 IdleTime = ?GV(idle_time, Conf),
    %                 timer:send_interval(IdleTime * 1000, dispatch_idle),
    %                 State = dispatch_idle(idle_check(State0)),
    %                 ?DBG({chat_started, Code}),
    %                 {ok, State}
    % 
    %         end;
    % 
    %     {error, Error} ->
    %         ?DBG({chat_start_failed, Code, Error}),
    %         {stop, Error}
    % end.
    
handle_call({join, _ParticipantUUID}, _From, State) ->    
    NewState = State,
    Reply = none,
    {reply, Reply, NewState};    
    % case glchat_mongo:get_chat(State#state.uuid, ParticipantUUID) of
    %     {ok, Participant} ->
    %         NewParts0 = ?PARTS:ensure(Participant, State#state.participants),
    %         NewParts = ?PARTS:commit(?PARTS:seq(ParticipantUUID, State#state.sequence, NewParts0)),
    %         NewState = State#state{participants=NewParts},
    %         Reply = [{userUUID, ?PARTS:get_field(ParticipantUUID, <<"session_uuid">>, NewParts)},
    %                  {chat, NewState#state.uuid},
    %                  {participants, ?PARTS:public_list(NewParts)},
    %                  {messages, [list_to_tuple(munge_message(M, NewParts)) 
    %                              || M <- lists:sublist(NewState#state.messages, ?BUFFER_SIZE)]}];
    %     {error, _Error} ->
    %         NewState = State,
    %         Reply = none
    % end,
    % {reply, Reply, NewState};

handle_call({subscribe, ParticipantUUID}, _From, State) ->
    participant_call(ParticipantUUID, State, fun handle_seq/2);
    
handle_call(Request, _From, State) ->
    ?DBG({unexpected_call, Request}),
    {reply, ok, State}.
    
handle_cast(Msg, State) ->
    ?DBG({unexpected_cast, Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    % Unexpected Input
    ?DBG({unexpected_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    % ?PARTS:commit(State#state.participants),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Participant-only method handlers
%%%===================================================================

participant_call(_ParticipantUUID, State, _Handler) ->
    % We can implement this. 
    {reply, {error, not_participant}, State}.
    
    % case ?PARTS:find(ParticipantUUID, State#state.participants) of
    %     error -> 
    %         {reply, {error, not_participant}, State};
    %     {ok, PartPL} -> 
    %         Handler(PartPL, State)
    % end.

handle_seq(PartPL, State) ->
    UserSeq = case dict:find(<<"seen_seq">>, PartPL) of
                  {ok, S} -> S;
                  error -> 0
              end,
    Diff = State#state.sequence - UserSeq,
    {reply, Diff, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% TODO