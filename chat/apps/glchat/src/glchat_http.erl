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
    implement_this,
    encode_reply(Req, [{success, true}, {uuid, "test_uuid"}, 
                       {chats, "test_chat"}, {subscribes, "test_info"}]);
    % create_session(Req);

%% -------------- Polling ----------------

handle('POST', [<<"poll">>], _Req) ->
    implement_this;
    % session_call(Req, [{<<"ack">>, int, false}], poll);

%% -------------- Chat API ----------------

% 1. message
% 2. history

%% -------------- Misc ----------------
    
handle(Method, Path, Req) ->
    ?DBG({not_found, Method, Path}),
    ?REQ:reply(404, [], "Page not found.", Req).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

% create_session(_Req) ->
%     case parse_args(Req, [{<<"pid">>, uuid, true}, 
%                           {<<"joins">>, {list, raw}, true},
%                           {<<"subscribes">>, {list, raw}, true}]) of
%         {ok, [ParticipantUUID, Joins, Subscribes]} ->
%             {ok, UUID, Pid} = glchat_sess:spawn_session(ParticipantUUID),
%             JoinInfos = gen_server:call(Pid, {call, join, [Joins]}),
%             SubscribeInfos = gen_server:call(Pid, {call, subscribe, [Subscribes]}),
%            encode_reply(Req, [{success, true}, {uuid, UUID}, 
%                               {chats, JoinInfos}, {subscribes, SubscribeInfos}]);
%         {error, Errors} ->
%             encode_reply(Req, [{success, false}, {error, invalid_call}, {call_errors, Errors}]);
%         Error ->
%             ?DBG({call_error, Error}),
%             send_error(Req, "Internal error")
%     end.


encode_reply(Req, PropList) ->
    JSON = jsonerl:encode(list_to_tuple(PropList)),
    send_reply(Req, JSON).

send_reply(Req, Reply) ->
    ?REQ:reply(200, [{'Content-Type', <<"application/json">>}], Reply, Req).
 
send_error(Req, Reply) ->
    ?REQ:reply(400, [{'Content-Type', <<"application/json">>}], Reply, Req).


% session_call(_Req, _ArgSpec, _Method) ->
%     implement_this.
%     case parse_args(Req, [{<<"id">>, uuid, true}|ArgSpec]) of
%         {ok, [SessionID|Args]} ->
%             case glchat_sess:find(SessionID) of
%                 undefined ->
%                     send_error(Req, "Session does not exist");
%                 Session ->
%                     Reply = case Method of
%                                 poll -> 
%                                     Ack = case Args of
%                                               [X] when is_integer(X) -> X;
%                                               _ -> -1
%                                           end,
%                                     {Seq, Messages} = gen_server:call(Session, {poll, Ack}, infinity),
%                                     {{seq, Seq}, {messages, [list_to_tuple(lists:ukeysort(1, M)) || M <- Messages]}};
%                                 _ ->
%                                     gen_server:call(Session, {call, Method, Args}, 30000)
%                             end,
%                     send_reply(Req, jsonerl:encode(Reply))
%             end;
%         {error, Errors} ->
%             encode_reply(Req, [{success, false}, {call_errors, Errors}]);
%         Error ->
%             ?DBG({call_error, Error}),
%             send_error(Req, "Internal error")
%     end.


% unquote(Bin) ->
%     list_to_binary(mochiweb_util:unquote(Bin)).
% 
% parse_args(Req, ArgSpec) ->
%     {Post, _} = ?REQ:body_qs(Req),
%     Decoded = [{unquote(K), unquote(V)} || {K, V} <- Post],
%     glchat_util:parse_args(ArgSpec, [], [], Decoded).
