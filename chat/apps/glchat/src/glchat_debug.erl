%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%% Debug log
%%% @end
%%% Created : 15th July 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_debug).

-behaviour(gen_server).

-include("glchat.hrl").

%% API
-export([start_link/0, log/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
  handle
}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

log(Term, Module) ->
    gen_server:cast(?SERVER, {log, self(), Term, Module}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Conf} = application:get_env(logging),
    Path = ?GV(log_file, Conf),
    
    {ok, Handle} = file:open(Path, [append, raw]),

    ?DBG("----------------------------------------"),
    ?DBG("Startup!"),
    {ok, #state{handle=Handle}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
        {reply, Reply, State}.


handle_cast({log, Pid, Term, Module}, State) ->
    {{Y, M, D}, {H,Min,S}} = calendar:universal_time(),
    Msg = io_lib:format("[~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B] [~p] ~p ~p~n", [Y, M, D, H, Min, S, Module, Pid, Term]),
    io:format(Msg),
    file:write(State#state.handle, Msg),
    {noreply, State};

handle_cast(Other, State) ->
    io:format("Unexpected: ~p~n", [Other]),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
