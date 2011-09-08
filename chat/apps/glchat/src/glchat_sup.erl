-module(glchat_sup).

-behaviour(supervisor).

-include("glchat.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    glchat_hunger:init(),

    ChatSup = {chat_sup, {glchat_chat_sup, start_link, []},
               permanent, 2000, supervisor, [glchat_chat_sup]},

    SessSup = {sess_sup, {glchat_sess_sup, start_link, []},
               permanent, 2000, supervisor, [glchat_sess_sup]},

    CacheSup = {cache_sup, {glchat_usercache_sup, start_link, []},
               permanent, 2000, supervisor, [glchat_usercache_sup]},

    {ok, { {one_for_one, 500, 10}, [ChatSup, SessSup, CacheSup]} }.

