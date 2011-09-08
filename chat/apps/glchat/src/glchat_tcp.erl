%%%-------------------------------------------------------------------
%%% @author  <brendonh@dev3.cogini.com>
%%% @copyright (C) 2011, 
%%% @doc
%%% TCP endpoint
%%% @end
%%% Created : 25 May 2011 by  <brendonh@dev3.cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_tcp).

-include("glchat.hrl").

%% API
-export([start_link/3, init/3]).

-export([bounce/1]).

-record(state, {
  socket,
  transport,
  opts,
  bouncer,
  participantUUID,
  userID = none,
  chatUUIDs
}).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Socket, Transport, Opts]),
    {ok, Pid}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

init(Socket, Transport, Opts) ->
    %?DBG({new_tcp_socket, Transport:peername(Socket)}),
    Transport:setopts(Socket, [{packet, 4}]),
    State = #state{socket=Socket, transport=Transport, opts=Opts},
    Bouncer = spawn_link(?MODULE, bounce, [State]),
    get_message(State#state{bouncer=Bouncer}).


get_message(State) ->
    Transport = State#state.transport,
    case Transport:recv(State#state.socket, 0, infinity) of
        {ok, Data} ->
            State#state.bouncer ! {in, Data},
            get_message(State);
        {error, timeout} -> 
            terminate(State);
        {error, closed} -> 
            terminate(State)
    end.


terminate(#state{socket=Socket, transport=Transport, bouncer=Bouncer}) ->
    %?DBG(tcp_socket_closed),
    Transport:close(Socket),
    Bouncer ! socket_closed,
    ok.


bounce(State) ->
    receive
        {in, Msg} ->
            case (catch jsonerl:decode(Msg)) of
                {'EXIT', _} ->
                    ?DBG({invalid_json, Msg});
                JSON when is_tuple(JSON) ->
                    NewState = handle_message(State, tuple_to_list(JSON)),
                    bounce(NewState);
                Other ->
                    ?DBG({invalid_message, Msg, Other}),
                    bounce(State)
            end;
        {push, Key, Obj} ->
            Msg = [{<<"chat">>, Key}|Obj],
            %?DBG({push, glchat_hunger:id_to_nick(State#state.userID), Msg}),
            send_message(State, Msg),
            bounce(State);
        socket_closed ->
            [(catch gproc:send({n, l, {chat, C}}, {connection_died, State#state.participantUUID}))
             || C <- State#state.chatUUIDs],
            ok;
        Other ->
            ?DBG({got, Other}),
            bounce(State)
    end.

send_message(State, JSON) when is_list(JSON) ->
    send_message(State, list_to_tuple(JSON));
send_message(#state{transport=Transport, socket=Socket}, JSON) ->
    Msg = jsonerl:encode(JSON),
    Transport:send(Socket, Msg).


handle_message(State, JSON) ->
    case glchat_util:parse_args(
           [{<<"message_id">>, int, true},
            {<<"method">>, bin, true}],
          [], [], JSON) of
        {ok, [MessageID, Method]} ->
            %?DBG({call, Method, MessageID}),
            handle_call(Method, MessageID, JSON, State);
        {error, Errors} ->
            ?DBG({invalid_call, Errors}),
            send_message(State, [{success, false}, {error, invalid_call}, {call_errors, Errors}]),
            State
    end.

handle_call(<<"amihungry">>, _MessageID, _JSON, #state{userID=none}=State) ->
    send_message(State, [{success, false}, {error, nouser}]),
    State;
handle_call(<<"amihungry">>, _MessageID, _JSON, #state{userID=UserID}=State) when UserID /= none ->
    Hungry = gproc:lookup_pids({p, l, {user_cache, UserID}}) /= [],
    send_message(State, [{success, true}, {hungry, Hungry}]),
    State;

handle_call(<<"subscribe">>, MessageID, _JSON, #state{participantUUID=UUID}=State)
  when is_binary(UUID) ->
    send_message(State, [{success, false}, {message_id, MessageID}, {error, duplicate_subscribe}]),
    State;

handle_call(<<"subscribe">>, MessageID, JSON, State) ->
    case glchat_util:parse_args([{<<"participant">>, uuid, true}, {<<"chats">>, list, true}], [], [], JSON) of
        {ok, [ParticipantUUID, ChatUUIDs]} ->
            {Infos, NewState} = start_session(State, ParticipantUUID, ChatUUIDs),
            send_message(NewState, [{success, true}, {message_id, MessageID}, 
                                    {chats, [list_to_tuple(I) || I <- Infos]}]),
            NewState#state{participantUUID=ParticipantUUID, chatUUIDs=ChatUUIDs};
        {error, Errors} ->
            send_message(State, [{success, false}, {message_id, MessageID}, {error, invalid_call}, {call_errors, Errors}]),
            State;
        Error ->
            ?DBG({call_error, Error}),
            send_message(State, [{success, false}, {message_id, MessageID}, {error, internal_error}]),
            State
    end;

handle_call(<<"message">>, MessageID, JSON, State) ->
    case glchat_util:parse_args([{<<"chat">>, chat, true}, {<<"message">>, bin, true}], [], [], JSON) of
        {ok, [Chat, Message]} ->
            Reply = glchat_sess_api:handle(message, [Chat, Message], State#state.participantUUID, State#state.userID),
            RPCReply = list_to_tuple([{message_id, MessageID} | tuple_to_list(Reply)]),
            send_message(State, RPCReply);
        {error, Errors} ->
            send_message(State, [{success, false}, {message_id, MessageID}, {error, invalid_call}, {call_errors, Errors}]);
        Error ->
            ?DBG({call_error, Error}),
            send_message(State, [{success, false}, {message_id, MessageID}, {error, internal_error}])
    end,
    State;


handle_call(<<"start_hungry">>, MessageID, _JSON, State) ->
    Reply = glchat_sess_api:handle(start_hungry, [], State#state.participantUUID, State#state.userID),
    RPCReply = list_to_tuple([{message_id, MessageID} | tuple_to_list(Reply)]),
    send_message(State, RPCReply),
    State;

handle_call(<<"get_hungry">>, MessageID, _JSON, State) ->
    Reply = glchat_sess_api:handle(get_hungry, [], State#state.participantUUID, State#state.userID),
    RPCReply = list_to_tuple([{message_id, MessageID} | tuple_to_list(Reply)]),
    send_message(State, RPCReply),
    State;

handle_call(<<"stop_hungry">>, MessageID, _JSON, State) ->
    Reply = glchat_sess_api:handle(stop_hungry, [], State#state.participantUUID, State#state.userID),
    RPCReply = list_to_tuple([{message_id, MessageID} | tuple_to_list(Reply)]),
    send_message(State, RPCReply),
    State;

handle_call(<<"create_chat">>, MessageID, JSON, State) ->
    case glchat_util:parse_args([{<<"f">>, bin, true}], [], [], JSON) of
        {ok, [FriendID]} ->
            Reply = glchat_sess_api:handle(create_chat, [FriendID], State#state.participantUUID, State#state.userID),
            RPCReply = list_to_tuple([{message_id, MessageID} | tuple_to_list(Reply)]),
            send_message(State, RPCReply);
        {error, Errors} ->
            send_message(State, [{success, false}, {message_id, MessageID}, {error, invalid_call}, {call_errors, Errors}]);
        Error ->
            ?DBG({call_error, Error}),
            send_message(State, [{success, false}, {message_id, MessageID}, {error, internal_error}])
    end,
    State;
    
handle_call(<<"join_chat">>, MessageID, JSON, State) ->
    case glchat_util:parse_args([{<<"chat">>, chat, true}], [], [], JSON) of
        {ok, [Chat]} ->
            Reply = glchat_sess_api:handle(join_chat, [Chat], State#state.participantUUID, State#state.userID),
            RPCReply = list_to_tuple([{message_id, MessageID} | tuple_to_list(Reply)]),
            send_message(State, RPCReply);
        {error, Errors} ->
            send_message(State, [{success, false}, {message_id, MessageID}, {error, invalid_call}, {call_errors, Errors}]);
        Error ->
            ?DBG({call_error, Error}),
            send_message(State, [{success, false}, {message_id, MessageID}, {error, internal_error}])
    end,
    State;
    


handle_call(Other, MessageID, _, State) ->
    ?DBG({unknown_call, Other}),
    send_message(State, [{success, false}, {message_id, MessageID}, {error, unknown_call}]).


start_session(State, ParticipantUUID, ChatUUIDs) ->
    gproc:reg({p, l, {participant_session, ParticipantUUID}}),
    UserID = case glchat_mongo:get_user_id(ParticipantUUID) of
                 none -> 
                     none;
                 UID ->
                     gproc:reg({p, l, {user_session, UID}}),
                     UID
             end,
    Infos = glchat_sess_api:handle(join, [ChatUUIDs], ParticipantUUID, UserID),
    {Infos, State#state{userID=UserID}}.

