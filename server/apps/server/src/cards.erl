-module(cards).
-include("cards.hrl").
-export([name/1, score/1, can_fish/2]).

name({#{symbol := Rank}, #{symbol := Suit}}) ->
    erlang:list_to_atom(lists:concat([Rank, Suit])).

score({Rank, Suit}) when Rank == ?RANK_TWO, Suit == ?SUIT_CLUBS -> 2;
score({Rank, Suit}) when Rank == ?RANK_TEN, Suit == ?SUIT_DIAMONDS -> 3;
score({Rank, _}) when Rank == ?RANK_JACK -> 1;
score({Rank, _}) when Rank == ?RANK_ACE -> 1;
score(_Card) -> 0.

can_fish({R1, _}, {_, _}) when R1 == ?RANK_JACK -> true;
can_fish({R, S}, {R, S}) -> true;
can_fish(_Card1, _Card2) -> false.
