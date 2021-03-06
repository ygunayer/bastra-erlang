-module(server_sup).
-export([init/1]).

-behaviour(supervisor).
-export([start_link/0]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    ChildSpecs = [
        #{id => game_sup, start => {server_game_sup, start_link, []}, restart => transient},
        #{id => gateway, start => {server_gateway, start_link, []}, restart => transient}
    ],
    {ok, {SupFlags, ChildSpecs}}.
