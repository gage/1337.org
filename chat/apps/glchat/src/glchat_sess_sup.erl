%%%-------------------------------------------------------------------
%%% @author  <gulu@dev3.cogini.com>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 11 May 2011 by  <gulu@dev3.cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_sess_sup).

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

    Spec = {sess, {glchat_sess, start_link, []},
            transient, 2000, worker, [glchat_sess]},

    {ok, {{simple_one_for_one, 600, 60}, [Spec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
