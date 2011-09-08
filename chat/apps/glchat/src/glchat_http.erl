%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, 
%%% @doc
%%% HTTP endpoint
%%% @end
%%% Created : 10 May 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_http).
-behaviour(cowboy_http_handler).

-include("glchat.hrl").

% API
-export([init/3, handle/2, terminate/2]).

-define(REQ, cowboy_http_req).

%%%===================================================================
%%% API functions
%%%===================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Tokens, Req2} = ?REQ:path(Req),
    {Method, Req3} = ?REQ:method(Req2),
    {ok, Req4} = handle(Method, Tokens, Req3),
    {ok, Req4, State}.

terminate(_Req, _State) ->    
    ok.


%%%===================================================================
%%% HTTP handlers
%%%===================================================================

%% -------------- Init ----------------

handle('POST', [<<"start">>], Req) ->
    create_session(Req);

%% -------------- Polling ----------------

handle('POST', [<<"poll">>], Req) ->
    session_call(Req, [{<<"ack">>, int, false}], poll);

%% -------------- Chat API ----------------

handle('POST', [<<"message">>], Req) ->
    session_call(Req, [{<<"c">>, chat, true}, {<<"m">>, bin, true}], message);

handle('POST', [<<"history">>], Req) ->
    session_call(Req, [{<<"c">>, chat, true}, 
                       {<<"start">>, int, true},
                       {<<"stop">>, int, true}], 
                 history);

%% -------------- Hunger API ----------------

handle('POST', [<<"start_hungry">>], Req) ->
    session_call(Req, [], start_hungry);

handle('POST', [<<"get_hungry">>], Req) ->
    session_call(Req, [], get_hungry);

handle('POST', [<<"stop_hungry">>], Req) ->
    session_call(Req, [], stop_hungry);

handle('POST', [<<"create_chat">>], Req) ->
    session_call(Req, [{<<"f">>, bin, true}], create_chat);

handle('POST', [<<"join_chat">>], Req) ->
    session_call(Req, [{<<"chat">>, chat, true}], join_chat);

handle('POST', [<<"subscribe_chat">>], Req) ->
    session_call(Req, [{<<"chat">>, chat, true}], subscribe_chat);

%% -------------- Misc ----------------
    
handle(Method, Path, Req) ->
    ?DBG({not_found, Method, Path}),
    ?REQ:reply(404, [], "Page not found.", Req).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

create_session(Req) ->
    case parse_args(Req, [{<<"pid">>, uuid, true}, 
                          {<<"joins">>, {list, raw}, true},
                          {<<"subscribes">>, {list, raw}, true}]) of
        {ok, [ParticipantUUID, Joins, Subscribes]} ->
            {ok, UUID, Pid} = glchat_sess:spawn_session(ParticipantUUID),
            JoinInfos = gen_server:call(Pid, {call, join, [Joins]}),
            SubscribeInfos = gen_server:call(Pid, {call, subscribe, [Subscribes]}),
            encode_reply(Req, [{success, true}, {uuid, UUID}, 
                               {chats, JoinInfos}, {subscribes, SubscribeInfos}]);
        {error, Errors} ->
            encode_reply(Req, [{success, false}, {error, invalid_call}, {call_errors, Errors}]);
        Error ->
            ?DBG({call_error, Error}),
            send_error(Req, "Internal error")
    end.


encode_reply(Req, PropList) ->
    JSON = jsonerl:encode(list_to_tuple(PropList)),
    send_reply(Req, JSON).

send_reply(Req, Reply) ->
    ?REQ:reply(200, [{'Content-Type', <<"application/json">>}], Reply, Req).

send_error(Req, Reply) ->
    ?REQ:reply(400, [{'Content-Type', <<"application/json">>}], Reply, Req).


session_call(Req, ArgSpec, Method) ->
    case parse_args(Req, [{<<"id">>, uuid, true}|ArgSpec]) of
        {ok, [SessionID|Args]} ->
            case glchat_sess:find(SessionID) of
                undefined ->
                    send_error(Req, "Session does not exist");
                Session ->
                    Reply = case Method of
                                poll -> 
                                    Ack = case Args of
                                              [X] when is_integer(X) -> X;
                                              _ -> -1
                                          end,
                                    {Seq, Messages} = gen_server:call(Session, {poll, Ack}, infinity),
                                    {{seq, Seq}, {messages, [list_to_tuple(lists:ukeysort(1, M)) || M <- Messages]}};
                                _ ->
                                    gen_server:call(Session, {call, Method, Args}, 30000)
                            end,
                    send_reply(Req, jsonerl:encode(Reply))
            end;
        {error, Errors} ->
            encode_reply(Req, [{success, false}, {call_errors, Errors}]);
        Error ->
            ?DBG({call_error, Error}),
            send_error(Req, "Internal error")
    end.


unquote(Bin) ->
    list_to_binary(mochiweb_util:unquote(Bin)).

parse_args(Req, ArgSpec) ->
    {Post, _} = ?REQ:body_qs(Req),
    Decoded = [{unquote(K), unquote(V)} || {K, V} <- Post],
    glchat_util:parse_args(ArgSpec, [], [], Decoded).
