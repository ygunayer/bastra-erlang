-module(server_gateway).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-define(SERVER_NAME, gateway).

start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, {}, []).

init(_Args) ->
    StateData = #{sessions => [], mmq => [], games => []},
    {ok, {ready, StateData}}.

handle_call(_Request, _From, State) ->
    {reply, {nack}, State}.

handle_cast({join, {Pid, Username}}, State = {ready, StateData}) ->
    #{sessions := Sessions, mmq := Queue, games := Games} = StateData,

    case lists:keyfind(Username, 1, Sessions) of
        false ->
            logger:debug("[gateway] Player ~s is joining with pid ~w", [Username, Pid]),

            Player = #{pid => Pid, username => Username},

            util:when_terminated(Pid,
                fun(_) ->
                    logger:debug("[gateway] Origin process of player ~s was terminated, disconnecting user", [Username]),
                    gen_server:cast(?SERVER_NAME, {leave, {Pid, Username}})
                end
            ),

            Pid ! {ack},

            % the naive matchmaking implementation
            NewStateData = case length(Queue) of
                1 ->
                    [OpponentUsername] = Queue,
                    {_, Opponent, _} = lists:keyfind(OpponentUsername, 1, Sessions),

                    {ok, GamePid} = server_game_sup:create_room([Player, Opponent]),
                    logger:debug("[gateway] Players ~s and ~s were matched up against each other in game ~w", [Username, OpponentUsername, GamePid]),

                    NewSessions = lists:keystore(
                        Username,
                        1,
                        lists:keystore(OpponentUsername, 1, Sessions, {OpponentUsername, Opponent, playing}),
                        {Username, Player, playing}
                    ),

                    #{
                        sessions => NewSessions,
                        mmq => [],
                        games => Games ++ [GamePid]
                    };
                _ ->
                    logger:debug("[gateway] Player ~s was added to the matchmaking queue", [Username]),
                    #{
                        sessions => lists:keystore(Username, 1, Sessions, {Username, Player, in_lobby}),
                        mmq => Queue ++ [Username],
                        games => Games
                    }
            end,

            {noreply, {ready, NewStateData}};

        _ ->
            logger:warning("[gateway] Prevented ~s from joining the game as they're already logged in", [Username]),
            Pid ! {error, already_logged_in},
            {noreply, State}
    end;
handle_cast({leave, {_Pid, Username}}, {ready, StateData}) ->
    #{sessions := Sessions, mmq := Queue} = StateData,
    NewSessions = lists:keydelete(Username, 1, Sessions),
    NewQueue = Queue -- [Username],
    NewStateData = maps:merge(StateData, #{sessions => NewSessions, mmq => NewQueue}),
    {noreply, {ready, NewStateData}};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({Pid, {join, Username}}, State) ->
    gen_server:cast(?SERVER_NAME, {join, {Pid, Username}}),
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.
