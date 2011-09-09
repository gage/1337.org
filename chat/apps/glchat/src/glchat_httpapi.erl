-module(glchat_httpapi).
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

%% -------------- Django ----------------

handle(_Method, [<<"broadcast">>], _Req) ->
    implement_this;
    % case parse_args(Method, Req, [{<<"chat">>, uuid, true},
    %                               {<<"message">>, json, true}]) of
    %     {ok, [ChatUUID, Message]} ->
    %         case glchat_chat_sup:ensure_chat(ChatUUID) of
    %             none ->
    %                 Reply = {{success, false}, {error, chat_not_found}},
    %                 send_error(Req, jsonerl:encode(Reply));
    %             Chat ->
    %                 case gen_server:call(Chat, {broadcast, Message}) of
    %                     {ok, Seq} ->
    %                         encode_reply(Req, [{success, true}, {seq, Seq}]);
    %                     Error ->
    %                         ?DBG({broadcast_error, Error}),
    %                         Reply = {{success, false}, {error, broadcast_error}},
    %                         send_error(Req, jsonerl:encode(Reply))
    %                 end
    %         end;
    %     {error, Errors} ->
    %         encode_reply(Req, [{success, false}, {error, invalid_call}, {call_errors, Errors}]);
    %     Error ->
    %         ?DBG({call_error, Error}),
    %         send_error(Req, "Internal error")
    % end;

handle(_Method, [<<"notify">>], _Req) ->
    implement_this;
    % case parse_args(Method, Req, [{<<"ids">>, jsonlist, true},
    %                               {<<"message">>, json, true}]) of
    %     {ok, [IDs, Message]} ->
    %         [gproc:send({p, l, {user_session, ID}}, {push, <<"notify">>, Message})
    %          || ID <- IDs],
    %         encode_reply(Req, [{success, true}]);
    %     {error, Errors} ->
    %         encode_reply(Req, [{success, false}, {error, invalid_call}, {call_errors, Errors}]);
    %     Error ->
    %         ?DBG({call_error, Error}),
    %         send_error(Req, "Internal error")
    % end;

%% -------------- Misc ----------------

handle(_Method, [<<"stats">>], _Req) ->
    implement_this;
    % Count = fun(Mod) -> ?GV(active, supervisor:count_children(Mod)) end,
    % Reply = jsonerl:encode(
    %           {{<<"chats">>, Count(glchat_chat_sup)},
    %            {<<"http_sessions">>, Count(glchat_sess_sup)},
    %            {<<"user_caches">>, Count(glchat_usercache_sup)}
    %           }),
    % ?REQ:reply(200, [{'Content-Type', <<"text/plain">>}], Reply, Req);


handle(Method, Path, Req) ->
    ?DBG({not_found, Method, Path}),
    ?REQ:reply(404, [], "Page not found.", Req).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

% encode_reply(Req, PropList) ->
%     JSON = jsonerl:encode(list_to_tuple(PropList)),
%     send_reply(Req, JSON).
% 
% send_reply(Req, Reply) ->
%     ?REQ:reply(200, [{'Content-Type', <<"text/plain">>}], Reply, Req).
% 
% send_error(Req, Reply) ->
%     ?REQ:reply(400, [{'Content-Type', <<"text/plain">>}], Reply, Req).
% 
% 
% unquote(Bin) ->
%     list_to_binary(mochiweb_util:unquote(Bin)).
% 
% parse_args('POST', Req, ArgSpec) ->
%     {Post, _} = ?REQ:body_qs(Req),
%     Decoded = [{unquote(K), unquote(V)} || {K, V} <- Post],
%     glchat_util:parse_args(ArgSpec, [], [], Decoded);
% parse_args('GET', Req, ArgSpec) ->
%     {Query, _} = ?REQ:qs_vals(Req),
%     Decoded = [{unquote(K), unquote(V)} || {K, V} <- Query],
%     glchat_util:parse_args(ArgSpec, [], [], Decoded).

