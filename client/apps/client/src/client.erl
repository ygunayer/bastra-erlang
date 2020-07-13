-module(client).
-export([start_link/0, init/0, connecting/1, in_lobby/1, playing/1]).

return_to_lobby(State) ->
    NewState = maps:merge(State, #{
        game => none,
        turn_info => #{}
    }),
    in_lobby(NewState).

start_link() ->
    Pid = spawn(?MODULE, init, []),
    {ok, Pid}.

init() ->
    {ok, ServerAddress} = application:get_env(client, server_address),
    GatewayRef = {gateway, ServerAddress},
    %Username = util:random_string(8),
    Username = io:get_line("Welcome to Bastra! Please enter your name."),
    io:format("SIKIK ~w~n", [Username]),
    State = #{
        gateway => GatewayRef,
        game => none,
        username => Username,
        turn_info => #{}
    },
    io:format("Connecting to the server at ~w~n", [ServerAddress]),
    connecting(State).

connecting(State = #{gateway := GatewayRef, username := Username}) ->
    {_, ServerAddress} = GatewayRef,
    case net_kernel:connect_node(ServerAddress) of
        true ->
            io:format("Connected to server, joining the lobby...~n"),
            GatewayRef ! {self(), {join, Username}},

            receive
                {ack} -> in_lobby(State)
            after 5000 ->
                io:format("Attempting to join the lobby has timed out after 5 seconds, retrying...~n"),
                connecting(State)
            end;
        _ ->
            io:format("Failed to reach the server, will try again in 5 seconds~n"),
            timer:sleep(5000),
            connecting(State)
    end.

in_lobby(State) ->
    io:format("You are now in the lobby~n"),
    receive
        {GamePid, join_game} ->
            io:format("You have joined the game ~w~n", [GamePid]),
            NewState = maps:put(game, GamePid, State),
            playing(NewState)
    end.

playing(State = #{game := GamePid}) ->
    receive
        {GamePid, terminate, Reason} ->
            io:format("The game has been terminated due to ~w, returning to the lobby.~n", [Reason]),
            return_to_lobby(State);

        {GamePid, in_turn, TurnInfo} ->
            #{hand := Hand, score := Score, top_card := TopCard, opponent_score := OpponentScore} = TurnInfo,

            io:format("It's your turn now. Your score: ~w, Opponent's Score: ~w~n", [Score, OpponentScore]),
            io:format("The top-most card is ~ts~n", [cards:name(TopCard)]),
            CardCount = lists:foldl(
                fun(Card, N) ->
                    io:format("~w. ~ts~n", [N, cards:name(Card)]),
                    N + 1
                end,
                1,
                Hand
            ),

            Anan = io:get_line(io_lib:format("Choose a card to play (1-~w)", [CardCount])),
            io:format("ANAN ~w~n", [Anan]),

            NewState = maps:put(turn_info, TurnInfo, State),
            playing(NewState);
            
        {GamePid, opponent_turn, TurnInfo = #{score := Score, opponent_score := OpponentScore}} ->
            io:format("It's the opponent's turn now. Your score: ~w, Opponent's Score: ~w~n", [Score, OpponentScore]),
            #{turn_info := OldTurnInfo} = State,
            NewTurnInfo = maps:merge(OldTurnInfo, TurnInfo),
            NewState = maps:put(turn_info, NewTurnInfo, State),
            playing(NewState);
        
        {GamePid, win, #{score := Score, opponent_score := OpponentScore}} ->
            io:format("You have won the game ~w to ~w~n", [Score, OpponentScore]),
            return_to_lobby(State);

        {GamePid, lose, #{score := Score, opponent_score := OpponentScore}} ->
            io:format("You have won the game ~w to ~w~n", [Score, OpponentScore]),
            return_to_lobby(State)
    end.
