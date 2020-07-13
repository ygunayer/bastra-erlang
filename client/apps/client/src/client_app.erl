-module(client_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Cookie} = application:get_env(client, cookie),
    Name = util:random_string(8),
    io:setopts([{encoding, unicode}]),
    NodeName = list_to_atom("client-" ++ Name ++ "@bastra.example"),
    io:format("Starting client with the name ~w~n", [NodeName]),
    {ok, _} = net_kernel:start([NodeName, longnames]),
    erlang:set_cookie(NodeName, Cookie),
    client_sup:start_link().

stop(_State) ->
    ok.
