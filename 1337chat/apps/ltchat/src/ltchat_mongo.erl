-module(ltchat_mongo).

-export([connect/0, get_conn/0]).

-include("ltchat.hrl").

connect() ->
    {ok, Conf} = application:get_env(mongo),
    Hosts = ?GV(hosts, Conf),
    DB = ?GV(db, Conf),
    mongodb:replicaSets(ltchat_mongo, Hosts),
    mongodb:connect(ltchat_mongo, self()),
    receive
        {mongodb_connected} ->
            Conn = mongoapi:new(ltchat_mongo, DB),
            Conn:set_encode_style(mochijson),

            application:set_env(ltchat, mongo_conn, Conn),            
            ?DBG({mongo, connected, Hosts, DB}),

            ok;
        Other ->
            ?DBG({unexpected_message, Other}),
            {error, unexpected}
    after 5000 ->
            ?DBG({mongo, connect_timeout}),
            {error, timeout}
    end.
    

get_conn() ->
    gproc:get_set_env(l, ltchat, mongo_conn, [app_env, error]).
