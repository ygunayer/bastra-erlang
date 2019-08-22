-module(server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Address} = application:get_env(server, address),
    {ok, Cookie} = application:get_env(server, cookie),
    {ok, _} = net_kernel:start([Address, longnames]),
    true = erlang:set_cookie(Address, Cookie),
    server_sup:start_link().

stop(_State) ->
    ok.
