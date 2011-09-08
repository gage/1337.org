%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, 
%%% @doc
%%% Mongo wrappers
%%% @end
%%% Created : 11 May 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_mongo).

-export([connect/0, get_conn/0,
         get_chat/1, get_chat/2,
         get_chat_by_session/1,
         commit_chat_changes/2,
         get_contact_email/1,
         get_user_email/1, get_user_id/1,
         get_device_token/1,
         get_hungry_chats/0, get_hungry_users/0
]).

-include("glchat.hrl").

connect() ->
    {ok, Conf} = application:get_env(mongo),
    Hosts = ?GV(hosts, Conf),
    DB = ?GV(db, Conf),
    %mongodb:singleServer(glchat_mongo, Host),
    mongodb:replicaSets(glchat_mongo, Hosts),
    mongodb:connect(glchat_mongo, self()),
    receive
        {mongodb_connected} ->
            Conn = mongoapi:new(glchat_mongo, DB),
            Conn:set_encode_style(mochijson),

            application:set_env(glchat, mongo_conn, Conn),            
            ?DBG({mongo, connected, Hosts, DB}),

            ok;
        Other ->
            ?DBG({unexpected_message, Other}),
            {error, unexpected}
    after 5000 ->
            ?DBG({mongo, connect_timeout}),
            {error, timeout}
    end.
    

get_conn() ->
    gproc:get_set_env(l, glchat, mongo_conn, [app_env, error]).


get_chat(ChatUUID) ->
    Conn = get_conn(),
    case Conn:findOne("chat_chat", 
                      [{<<"uuid">>, ChatUUID},
                       {<<"finished">>, {ne, true}}]) of
        {ok, [_|_]=Doc} -> {ok, Doc};
        {ok, []} -> {error, not_found}
    end.
    

get_chat(ChatUUID, ParticipantUUID) ->
    Conn = get_conn(),
    PartKey = <<"participants.", ParticipantUUID/binary>>,
    case Conn:findOne("chat_chat", 
                      [{<<"uuid">>, ChatUUID}, 
                       {PartKey, {exists, true}}],
                      [{PartKey, true}]) of
        {ok, [_|_]=Doc} -> 
            [{ParticipantUUID, Participant}|_] = ?GV(<<"participants">>, Doc),
            {ok, [{<<"chat">>, ChatUUID}|Participant]};
        _Other ->
            {error, not_participant}
    end.


get_hungry_chats() ->
    Conn = get_conn(),
    {ok, Docs} = Conn:find("chat_chat", 
                           [{<<"hungry">>, true}, {<<"finished">>, {ne, true}}], 
                           [{<<"uuid">>, true}], 0, 0),
    [?GV(<<"uuid">>, Doc) || Doc <- Docs].

get_hungry_users() ->
    Conn = get_conn(),
    {ok, Docs} = Conn:find("user_profiles_userprofile", 
                           [{<<"hungry">>, true}], 
                           [{<<"user_id">>, true}], 0, 0),
    [UID || {oid, UID} <- [?GV(<<"user_id">>, Doc) || Doc <- Docs]].


get_chat_by_session(SessionUUID) ->
    Conn = get_conn(),
    case Conn:findOne("chat_chat",
                      [{<<"sessions.session">>, SessionUUID}],
                      [{<<"uuid">>, true}, {<<"sessions">>, true}]) of
        {ok, [_|_]=Doc} ->
            ChatUUID = ?GV(<<"uuid">>, Doc),

            %% https://jira.mongodb.org/browse/SERVER-828
            {array, Sessions} = ?GV(<<"sessions">>, Doc),
            {ok, Session} = glchat_util:find_object(Sessions, <<"session">>, SessionUUID),
            ParticipantUUID = ?GV(<<"participant">>, Session),
            {ok, ChatUUID, ParticipantUUID};
        _Other ->
            {error, not_found}
    end.    

commit_chat_changes(_ChatUUID, []) -> ok;
commit_chat_changes(ChatUUID, Changes) ->
    case get(trace) of true -> ?DBG({changes, Changes}); _ -> ok end,
    Conn = get_conn(),
    Conn:update("chat_chat", [{<<"uuid">>, ChatUUID}],
                Changes, []),
    ok.  


get_user_id(ParticipantUUID) ->
    Conn = get_conn(),
    case Conn:findOne("user_profiles_userprofile",
                      [{<<"uuid">>, ParticipantUUID}],
                      [{<<"user_id">>, true}]) of
        {ok, [_|_]=Doc} ->
            {oid, UserID} = ?GV(<<"user_id">>, Doc),
            UserID;
        _ ->
            none
    end.


get_device_token(UserID) ->
    Conn = get_conn(),
    case Conn:findOne("user_profiles_userprofile",
                      [{<<"user_id">>, {oid, UserID}}],
                      [{<<"iphone_device_token">>, true}]) of
        {ok, [_|_]=Doc} ->
            ?GV(<<"iphone_device_token">>, Doc);
        _ ->
            none
    end.


get_contact_email(ContactID) ->    
    get_email("contact_contact", ContactID).

get_user_email(ContactID) ->    
    get_email("auth_user", ContactID).


get_email(Collection, ID) ->
    Conn = get_conn(),
    case Conn:findOne(Collection,
                      [{<<"_id">>, ID}],
                      [{<<"email">>, true}]) of
        {ok, [_|_]=Doc} ->
            ?GV(<<"email">>, Doc);
        _ ->
            none
    end.