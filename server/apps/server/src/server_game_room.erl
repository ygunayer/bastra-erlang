-module(server_game_room).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Public API
%% -------------------
start_link(Players) ->
    gen_server:start_link(?MODULE, {Players}, []).

%% Private API
%% -------------------
inform_game_state(Game = #{id := GameId}) ->
    #{currentPlayer := Current, nextPlayer := Opponent, middle := MidStack} = Game,
    #{identity := #{pid := PidCurrent}, hand := HandCurrent, score := ScoreCurrent} = Current,
    #{identity := #{pid := PidOpponent}, score := ScoreOpponent} = Opponent,

    TopCard = case length(MidStack) of
        N when N < 2 -> none;
        _ -> lists:nth(1, MidStack)
    end,

    PidCurrent ! {
        self(),
        in_turn,
        #{
            hand => HandCurrent,
            score => ScoreCurrent,
            top_card => TopCard,
            opponent_score => ScoreOpponent
        }
    },

    PidOpponent ! {self(), opponent_turn, #{score => ScoreOpponent, opponent_score => ScoreCurrent}},

    logger:debug("[~s] Players were informed of the new state", [GameId]).

%% Callback Implementation
%% -------------------
init({[P1, P2]}) ->
    #{pid := Pid1} = P1,
    #{pid := Pid2} = P2,
    Id = "game:" ++ util:random_string(8),
    Game = bastra_game:new([P1, P2]),
    Pid1 ! {self(), join_game},
    Pid2 ! {self(), join_game},

    {ok, {initializing, #{id => Id, game => Game}}}.

handle_call(_Request, _From, State) ->
    {reply, {nack}, State}.

handle_cast({start}, {initializing, StateData}) ->
    #{game := GameBefore} = StateData,

    GameNow = bastra_game:start(GameBefore),

    #{currentPlayer := #{identity := #{pid := PidCurrent}}} = GameNow,

    inform_game_state(GameBefore),

    NewStateData = maps:merge(StateData, #{game => GameNow}),
    {noreply, {in_turn, PidCurrent, NewStateData}};
handle_cast({play, Sender, CardNo}, {in_turn, PidCurrent, StateData}) when PidCurrent == Sender ->
    #{game := GameBefore} = StateData,
    NewStateData =
        case bastra_game:play_card(CardNo, GameBefore) of
            illegal ->
                Sender ! {nack},
                StateData;
            GameNow ->
                Sender ! {ack},
                inform_game_state(GameNow),
                GameNow
        end,
    {noreply, {in_turn, PidCurrent, NewStateData}};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.
