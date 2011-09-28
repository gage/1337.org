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
            
            % Seq = case ?GV(<<"last_message_sequence">>, Info) of
            %           undefined -> 0;
            %           Val -> round(Val)
            %       end,
            %                        
            Seq = 0,
            Messages = [],
            Title = "tmp title",
            URL = "tmp url",
            Participants = ?GVD(<<"participants">>, Info, []),
            % {array, Messages} = ?GVD(<<"messages">>, Info, {array, []}),
            % 
            % Title = case ?GV(<<"title">>, Info) of
            %             undefined -> <<"Update: ", UUID/binary>>;
            %             "" -> <<"Update: ", UUID/binary>>;
            %             TitleStr -> TitleStr
            %         end,
            % 
            % URL = case ?GV(<<"url">>, Info) of
            %           undefined -> undefined;
            %           "" -> undefined;
            %           URLStr -> URLStr
            %       end,
            % 
            State0 = #state{
              uuid = UUID,
              messages = lists:reverse(Messages),
              participants = ?PARTS:new(Participants),
              hungry = ?GVD(<<"hungry">>, Info, false),
              title = Title,
              url = URL,
              sequence = Seq
            },
            timer:send_interval(?IDLE_CHECK_TIME, idle_check),
            IdleTime = ?GV(idle_time, Conf),
            timer:send_interval(IdleTime * 1000, dispatch_idle),
            State = dispatch_idle(idle_check(State0)),
            ?DBG({chat_started, Code}),
            {ok, State};
            % case State0#state.hungry == true
            %     andalso length(?PARTS:hungry_list(State0#state.participants)) < 2 of
            %     true -> 
            %         ?DBG({chat_finished, State0#state.uuid}),
            %         ?PARTS:commit(State0#state.participants, [{<<"finished">>, {set, true}}]),
            %         {stop, normal};
            %     _ -> 
            %         timer:send_interval(?IDLE_CHECK_TIME, idle_check),
            %         IdleTime = ?GV(idle_time, Conf),
            %         timer:send_interval(IdleTime * 1000, dispatch_idle),
            %         State = dispatch_idle(idle_check(State0)),
            %         ?DBG({chat_started, Code}),
            %         {ok, State}
            % 
            % end;

        {error, Error} ->
            ?DBG({chat_start_failed, Code, Error}),
            {stop, Error}
    end.
    
handle_call({join, ParticipantUUID}, _From, State) ->
    case glchat_mongo:get_chat(State#state.uuid, ParticipantUUID) of
        {ok, Participant} ->
            NewParts = ?PARTS:ensure(Participant, State#state.participants),
            % NewParts = ?PARTS:commit(?PARTS:seq(ParticipantUUID, State#state.sequence, NewParts0)),
            NewState = State#state{participants=NewParts},
            Reply = [{userUUID, ?PARTS:get_field(ParticipantUUID, <<"session_uuid">>, NewParts)},
                     {chat, NewState#state.uuid},
                     {participants, ?PARTS:public_list(NewParts)},
                     % {messages, [list_to_tuple(munge_message(M, NewParts)) 
                     %             || M <- lists:sublist(NewState#state.messages, ?BUFFER_SIZE)]}];
                     {messages, []}];
            
        {error, _Error} ->
            NewState = State,
            Reply = none
    end,
    {reply, Reply, NewState};
% 
% handle_call({subscribe, ParticipantUUID}, _From, State) ->
%     participant_call(ParticipantUUID, State, fun handle_seq/2);


handle_call({message, ParticipantUUID, Content}, _From, State) ->
    handle_call({message, ParticipantUUID, Content, true}, _From, State);
handle_call({message, ParticipantUUID, Content, Touch}, _From, State) ->
    participant_call(ParticipantUUID, State, 
                     fun(PartPL, _S) -> 
                             handle_message(PartPL, ParticipantUUID, Content, Touch, State)
                     end);
    
handle_call(Request, _From, State) ->
    ?DBG({unexpected_call, Request}),
    {reply, ok, State}.
    
handle_cast(Msg, State) ->
    ?DBG({unexpected_cast, Msg}),
    {noreply, State}.

handle_info(idle_check, State) ->
    {noreply, idle_check(State)};

handle_info(dispatch_idle, State) ->
    {noreply, dispatch_idle(State)};

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

participant_call(ParticipantUUID, State, Handler) ->
    case ?PARTS:find(ParticipantUUID, State#state.participants) of
        error -> 
            {reply, {error, not_participant}, State};
        {ok, PartPL} -> 
            Handler(PartPL, State)
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
    % Here we should do message logging and sequence process.

    Title = State#state.title,
    NewParts = State#state.participants,
    % I just cannot understand the line below
    ?DBG({process_id, gproc:lookup_pids({p, l, UUID})}),
    gproc:send({p, l, UUID}, {push, {message, UUID, Seq}, munge_message(Message, NewParts)}),

    NewState = State#state{
                 sequence=Seq,
                 participants=NewParts,
                 messages=[Message|State#state.messages]},

    {reply, {ok, Seq}, NewState}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

% Process Message
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

% TODO
idle_check(State) ->
    {ok, Conf} = application:get_env(chat),
    InactiveTime = ?GV(inactive_time, Conf),
    % NewParts = ?PARTS:set_unseen_to_inactive(InactiveTime, State#state.participants),
    % State#state{participants=NewParts}.
    State.

dispatch_idle(State) ->
    {ok, Conf} = application:get_env(chat),
    IdleTime = ?GV(idle_time, Conf),
    % NewParts = ?PARTS:dispatch_idle(IdleTime, State#state.messages, 
    %                                 State#state.title, 
    %                                 [{url, State#state.url},
    %                                  {title, State#state.title}],
    %                                 State#state.participants),
    % State#state{participants=?PARTS:commit(NewParts)}.
    State.


    