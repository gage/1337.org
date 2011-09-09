-module(glchat_chat_sup).

-behaviour(supervisor).

-include("glchat.hrl").

%% API
-export([start_link/0, ensure_chat/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


ensure_chat(_Code) ->
    implement_this.
    % case gproc:where({n, l, {chat, Code}}) of
    %     undefined ->
    %         case supervisor:start_child(?SERVER, [Code]) of
    %             {ok, Pid} -> Pid;
    %             Other -> 
    %                 ?DBG({oh_noes,Other}),
    %                 none
    %         end;
    %     Pid ->
    %         Pid
    % end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->

    Spec = {sess, {glchat_chat, start_link, []},
            transient, 2000, worker, [glchat_sess]},

    {ok, {{simple_one_for_one, 1, 60}, [Spec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
