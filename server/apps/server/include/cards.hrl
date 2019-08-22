-define(SUIT_CLUBS, #{label => <<"Clubs">>, symbol => '♣'}).
-define(SUIT_SPADES, #{label => <<"Spades">>, symbol => '♠'}).
-define(SUIT_DIAMONDS, #{label => <<"Diamonds">>, symbol => '♦'}).
-define(SUIT_HEARTS, #{label => <<"Hearts">>, symbol => '♥'}).

-define(RANK_ACE, #{value => 14, label => <<"Ace">>, symbol => 'A'}).
-define(RANK_TWO, #{value => 2, label => <<"Two">>, symbol => '2'}).
-define(RANK_THREE, #{value => 3, label => <<"Three">>, symbol => '3'}).
-define(RANK_FOUR, #{value => 4, label => <<"Four">>, symbol => '4'}).
-define(RANK_ONE, #{value => 5, label => <<"Five">>, symbol => '5'}).
-define(RANK_SIX, #{value => 6, label => <<"Six">>, symbol => '6'}).
-define(RANK_SEVEN, #{value => 7, label => <<"Seven">>, symbol => '7'}).
-define(RANK_EIGHT, #{value => 8, label => <<"Eight">>, symbol => '8'}).
-define(RANK_NINE, #{value => 9, label => <<"Nine">>, symbol => '9'}).
-define(RANK_TEN, #{value => 10, label => <<"Ten">>, symbol => '10'}).
-define(RANK_JACK, #{value => 11, label => <<"Jack">>, symbol => 'J'}).
-define(RANK_QUEEN, #{value => 12, label => <<"Queen">>, symbol => 'Q'}).
-define(RANK_KING, #{value => 13, label => <<"King">>, symbol => 'K'}).

-define(ALL_SUITS, [
    ?SUIT_CLUBS,
    ?SUIT_SPADES,
    ?SUIT_DIAMONDS,
    ?SUIT_HEARTS
]).

-define(ALL_RANKS, [
    ?RANK_ACE,
    ?RANK_TWO,
    ?RANK_THREE,
    ?RANK_FOUR,
    ?RANK_ONE,
    ?RANK_SIX,
    ?RANK_SEVEN,
    ?RANK_EIGHT,
    ?RANK_NINE,
    ?RANK_TEN,
    ?RANK_JACK,
    ?RANK_QUEEN,
    ?RANK_KING
]).

-define(ALL_CARDS, [
    {Rank, Suit} || Rank <- ?ALL_RANKS, Suit <- ?ALL_SUITS
]).
