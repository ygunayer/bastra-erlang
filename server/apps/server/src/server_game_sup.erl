-module(server_game_sup).
-export([start_link/0, create_room/1]).

-behaviour(supervisor).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_room(Players) ->
    supervisor:start_child(?SERVER, [Players]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
    ChildSpecs = [#{id => poll, start => {server_game_room, start_link, []}}],
    logger:debug("[supervisor:game] Game room supervisor is launching"),
    {ok, {SupFlags, ChildSpecs}}.
