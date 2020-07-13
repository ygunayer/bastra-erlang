-module(bastra_game).
-include("cards.hrl").
-export([new/1, start/1, play_card/2]).

new([P1, P2]) ->
    Id = "game:" ++ util:random_string(8),

    Deck = util:shuffle(?ALL_CARDS ++ ?ALL_CARDS),

    {Current, Next} =
        case rand:uniform(2) of
            1 -> {P1, P2};
            _ -> {P2, P1}
        end,

    #{
        id => Id,
        status => starting,
        currentPlayer => #{
            identity => Current,
            hand => [],
            bucket => [],
            score => #{
                total => 0,
                bastras => 0,
                fished => 0
            }
        },

        nextPlayer => #{
            identity => Next,
            hand => [],
            bucket => [],
            score => #{
                total => 0,
                bastras => 0,
                fished => 0
            }
        },

        middle => [],
        rest => Deck
    }.

start(Game = #{status := starting, rest := Deck}) ->
    {Hand1, L1} = lists:split(4, Deck),
    {Hand2, L2} = lists:split(4, L1),
    {Middle, Rest} = lists:split(4, L2),
    
    #{currentPlayer := Current, nextPlayer := Next} = Game,

    CurrentNew = maps:merge(Current, #{hand => Hand1}),
    NextNew = maps:merge(Next, #{hand => Hand2}),

    maps:merge(Game, #{
        status => playing,
        currentPlayer => CurrentNew,
        nextPlayer => NextNew,
        middle => Middle,
        rest => Rest
    }).

play_card(CardNo, Game = #{startus := playing}) ->
    illegal.
    %Game.
