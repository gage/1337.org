-module(ltchat_sess_sup).

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

    Spec = {sess, {ltchat_sess, start_link, []},
            transient, 2000, worker, [ltchat_sess]},

    {ok, {{simple_one_for_one, 600, 60}, [Spec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
