-module(glchat_sess_api).

-behaviour(glchat_sess).

-include("glchat.hrl").

-export([handle/4]).


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
             
handle(Method, Args, _ParticipantUUID, _UserID) ->
    ?DBG({unknown_api_call, Method, Args}),
    {{success, false}, {error, unknown_call}}.

get_info(ChatUUID, ParticipantUUID) ->
    % Seems like if there already is a reg for the same key-value grpoc will throw a exception.
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
