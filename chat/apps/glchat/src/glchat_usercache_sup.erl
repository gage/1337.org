%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%% User cache supervisor
%%% @end
%%% Created : 13 Jul 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_usercache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Spec = {cache, {glchat_usercache, start_link, []},
            transient, 2000, worker, [glchat_usercache]},

    {ok, {{simple_one_for_one, 600, 60}, [Spec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
