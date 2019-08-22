-module(util).
-export([random_string/1, random_string/2]).

random_string(Length) ->
    random_string(Length, "abcdefghijklmnopqrstuvwxyz1234567890").

random_string(Length, AllowedChars) ->
    lists:foldl(
        fun(_, Acc) ->
            [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
        end,
        [],
        lists:seq(1, Length)
    ).
