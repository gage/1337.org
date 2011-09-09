-module(glchat_sess_api).

-behaviour(glchat_sess).

-include("glchat.hrl").

-export([handle/4]).


handle(join, [_ChatUUIDs], _ParticipantUUID, _UserID) ->
    implement_this;
    % [Info || Info <- [get_info(ChatUUID, ParticipantUUID) 
    %                   || ChatUUID <- ChatUUIDs,
    %                      ChatUUID /= <<>>], 
    %          Info /= none];

% 1. subscribe
% 2. message
% 3. create_chat
% 4. join_chat
% 5. subscribe_chat
% 6. history

handle(Method, Args, _ParticipantUUID, _UserID) ->
    ?DBG({unknown_api_call, Method, Args}),
    {{success, false}, {error, unknown_call}}.

