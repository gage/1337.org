%%%-------------------------------------------------------------------
%%% @author  <gulu@dev3.cogini.com>
%%% @copyright (C) 2011, 
%%% @doc
%%% Application-level API for user sessions
%%% @end
%%% Created : 17 May 2011 by  <gulu@dev3.cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_sess_api).

-behaviour(glchat_sess).

-include("glchat.hrl").

-export([handle/4]).

-define(UN(ID), glchat_hunger:id_to_nick(ID)).
-define(UNS(IDs), [glchat_hunger:id_to_nick(U) || U <- IDs]).


handle(join, [ChatUUIDs], ParticipantUUID, _UserID) ->
    [Info || Info <- [get_info(ChatUUID, ParticipantUUID) 
                      || ChatUUID <- ChatUUIDs,
                         ChatUUID /= <<>>], 
             Info /= none];

handle(subscribe, [ChatUUIDs], ParticipantUUID, _UUID) ->
    [Info || Info <- [subscribe(ChatUUID, ParticipantUUID)
                      || ChatUUID <- ChatUUIDs,
                         ChatUUID /= <<>>],
             Info /= none];

handle(message, [{_ChatUUID, Chat}, Message], ParticipantUUID, _UserID) ->
    case gen_server:call(Chat, {message, ParticipantUUID, Message}, 30000) of
        {ok, Seq} ->
            {{success, true}, {sequence, Seq}};
        {error, Error} ->
            {{success, false}, {error, Error}}
    end;

handle(start_hungry, [], ParticipantUUID, none) ->
    ?DBG({illegal_start_hungry, no_user, ParticipantUUID}),
    {{success, false}, {error, no_user}};
handle(start_hungry, [], _ParticipantUUID, UserID) ->
    {Chats, Friends} = glchat_hunger:start_hungry(UserID),
    {{success, true}, 
     {chats, [munge_hungry_chat(C) || C <- Chats]}, 
     {friends, list_to_tuple(Friends)}};

handle(get_hungry, [], ParticipantUUID, none) ->
    ?DBG({illegal_get_hungry, no_user, ParticipantUUID}),
    {{success, false}, {error, no_user}};
handle(get_hungry, [], _ParticipantUUID, UserID) ->
    {Chats, Friends} = glchat_hunger:get_hungry(UserID),
    {{success, true}, 
     {chats, [munge_hungry_chat(C) || C <- Chats]}, 
     {friends, list_to_tuple(Friends)}};

handle(stop_hungry, [], ParticipantUUID, none) ->
    ?DBG({illegal_stop_hungry, no_user, ParticipantUUID}),
    {{success, false}, {error, no_user}};
handle(stop_hungry, [], _ParticipantUUID, UserID) ->
    glchat_hunger:stop_hungry(UserID),
    {{success, true}};

handle(create_chat, _, ParticipantUUID, none) ->
    ?DBG({illegal_create_chat, no_user, ParticipantUUID}),
    {{success, false}, {error, no_user}};
handle(create_chat, [UserID], _, UserID) ->
    ?DBG({illegal_create_chat, same_user, UserID}),
    {{success, false}, {error, same_user}};
handle(create_chat, [FriendID], ParticipantUUID, UserID) ->
    case (catch glchat_hunger:create_hungry_chat(UserID, FriendID)) of
        ChatUUID when is_binary(ChatUUID) ->
            Info = get_info(ChatUUID, ParticipantUUID),
            {{success, true}, {chat, list_to_tuple(Info)}};
        Error ->
            ?DBG({join_chat_error, Error}),
            {{success, false}}
    end;

handle(join_chat, _, ParticipantUUID, none) ->
    ?DBG({illegal_join_chat, no_user, ParticipantUUID}),
    {{success, false}, {error, no_user}};
handle(join_chat, [{ChatUUID, _Chat}], ParticipantUUID, UserID) ->
    case (catch glchat_hunger:join_hungry_chat(UserID, ChatUUID)) of
        ok ->
            Info = get_info(ChatUUID, ParticipantUUID),
            {{success, true}, {chat, list_to_tuple(Info)}};
        Error ->
            ?DBG({join_chat_error, Error}),
            {{success, false}}
    end;


handle(subscribe_chat, [{ChatUUID, _Chat}], ParticipantUUID, _UserID) ->
    Info = get_info(ChatUUID, ParticipantUUID),
    {{success, true}, {chat, list_to_tuple(Info)}};
    

handle(history, [{_ChatUUID, Chat}, Start, Stop], ParticipantUUID, _UserID) ->
    History = gen_server:call(Chat, {history, Start, Stop, ParticipantUUID}),
    {{success, true}, {history, History}};


handle(Method, Args, _ParticipantUUID, _UserID) ->
    ?DBG({unknown_api_call, Method, Args}),
    {{success, false}, {error, unknown_call}}.



get_info(ChatUUID, ParticipantUUID) ->
    catch gproc:reg({p, l, ChatUUID}),
    case glchat_chat_sup:ensure_chat(ChatUUID) of
        none -> none;
        Chat ->
            gen_server:call(Chat, {join, ParticipantUUID}, 30000)
    end.


subscribe(ChatUUID, ParticipantUUID) ->
    catch gproc:reg({p, l, ChatUUID}),
    case glchat_chat_sup:ensure_chat(ChatUUID) of
        none -> none;
        Chat ->
            SeqDiff = gen_server:call(Chat, {subscribe, ParticipantUUID}, 30000),
            {{chat, ChatUUID}, {seq_diff, SeqDiff}}
    end.

munge_hungry_chat(ChatUUID) ->
    Chat = glchat_chat_sup:ensure_chat(ChatUUID),
    Parts = [{{<<"user_id">>, UID},
              {<<"display_name">>, Name}}
             || {UID, Name} <- gen_server:call(Chat, partlist)],

    {{<<"chat">>, ChatUUID},
     {<<"participants">>, Parts}}.
                    
