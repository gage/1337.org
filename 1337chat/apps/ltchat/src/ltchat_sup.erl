-module(ltchat_sup).

-behaviour(supervisor).

-include("ltchat.hrl").

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

    % ChatSup = {chat_sup, {glchat_chat_sup, start_link, []},
    %            permanent, 2000, supervisor, [glchat_chat_sup]},
    % 
    SessSup = {sess_sup, {ltchat_sess_sup, start_link, []},
               permanent, 2000, supervisor, [ltchat_sess_sup]},

    {ok, { {one_for_one, 500, 10}, [SessSup]} }.

