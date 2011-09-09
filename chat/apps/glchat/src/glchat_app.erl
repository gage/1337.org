-module(glchat_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-include("glchat.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(lager),
    ok = application:start(crypto),
    ok = application:start(gproc),
    ok = application:start(erlmongo),
    ok = application:start(cowboy),
    ok = application:start(glchat).

start(_StartType, _StartArgs) ->
    ok = glchat_mongo:connect(),

    {ok, SupPid} = glchat_sup:start_link(),

    {ok, HTTPConfig} = application:get_env(http),

    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port, ?GV(port, HTTPConfig)}],
                          cowboy_http_protocol, ?GV(proto_opts, HTTPConfig)),

    {ok, HTTPAPIConfig} = application:get_env(httpapi),

    cowboy:start_listener(httpapi, 10,
                          cowboy_tcp_transport, [{port, ?GV(port, HTTPAPIConfig)}],
                          cowboy_http_protocol, ?GV(proto_opts, HTTPAPIConfig)),

    {ok, SupPid}.


stop(_State) ->
    ok.
