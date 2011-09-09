-module(glchat_participants).

-include("glchat.hrl").

-export([]).

-record(pstate, {
  parts
}).



% TODO: This is a file to operate like there is a glchat_participants model
% 1. new user
% 2. find user
% 3. add user
% 4. ensure user
% 5. remove user
% 6. touch&untouch (dont know what)
% 7. userid_list (??)
% 8. pushnotify

%%%===================================================================
%%% Internal functions
%%%===================================================================

% 1. broadcast_user
