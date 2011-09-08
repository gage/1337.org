%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%% 
%%% @end
%%% Created : 14 Jul 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_apns).

-include("glchat.hrl").
-include("../../../deps/apns/include/apns.hrl").

-export([init/0, ensure_connection/0, send/5, test/0]).

% Alan
-define(KEY, "c56e2a8001b8e1de3dbd769354c7f19ae525fde71d120f3db6e1bca8992f73cf").

-define(EXPIRY, 30 * 60 * 1000000).

init() ->
    {ok, Conn} = apns:connect(glchat_apns_conn),
    ?DBG({apns_conn, Conn}),
    ets:new(glchat_apns, [public, set, named_table]),
    ok.


ensure_connection() ->
    case apns:connect(glchat_apns_conn) of 
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        Other ->
            ?DBG({apns_connect_error, Other}),
            error
    end.
                                                   


send(UserID, ChatUUID, Title, DisplayName, Message) ->
    Token = get_token(UserID),
    do_send(Token, UserID, ChatUUID, Title, DisplayName, Message).


get_token(UserID) ->
    case ets:lookup(glchat_apns, UserID) of
        [] -> 
            load_token(UserID);
        [{UserID, {Token, SetTime}}] ->
            check_cached_token(UserID, Token, SetTime)
    end.
    

load_token(UserID) ->
    Token = case glchat_mongo:get_device_token(UserID) of
                <<>> -> no_id;
                Tok when is_binary(Tok) -> binary_to_list(Tok);
                _ -> no_id
            end,
    %?DBG({setting_device, glchat_hunger:id_to_nick(UserID), Token}),
    ets:insert(glchat_apns, {UserID, {Token, now()}}),
    Token.


check_cached_token(UserID, Token, SetTime) ->
    case timer:now_diff(now(), SetTime) of
        X when X > ?EXPIRY ->
            %?DBG({expiring, UserID, Token}),
            ets:delete(glchat_apns, UserID),
            load_token(UserID);
        _ ->
            Token
    end.



do_send(no_id, _UserID, _, _, _, _) ->
    ok;
do_send(Token, _UserID, ChatUUID, Title, DisplayName, Message) ->
    MessageString0 = case Title of
                         none -> [DisplayName, Message];
                         _ -> [Title, $\n, DisplayName, Message]
                     end,
    MessageString = lists:flatten(MessageString0),
    Extra = case ChatUUID of
                none -> [];
                _ ->
                    URL = <<"guluapp://chat/", ChatUUID/binary>>,
                    [{<<"chat">>, URL}]
            end,

    Msg = #apns_msg{
                device_token = Token,
                alert = MessageString,
                extra = Extra},
    Result = apns:send_message(
               glchat_apns_conn,
               Msg),
    ?DBG({msg, Msg, Result}),
    ok.

test() ->
    Alan = <<"98027c33-431a-4632-a5bb-f7502d1e3676">>,
    %Ryan = <<"231e39b9-aa11-475c-920d-8f84007137da">>,
    Chat = <<"dc56c37c-9021-4a25-869f-5f30fd4483ec">>,
    send(Alan, Chat, <<>>, "Brend", "HELLO HOW ARE YOU").
