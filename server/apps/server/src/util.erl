-module(util).
-export([shuffle/1, random_string/1, random_string/2, when_terminated/2]).

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

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

when_terminated(Pid, FnOnTerminate) ->
    spawn(fun() ->
        erlang:monitor(process, Pid),
        receive
            Reason = {'DOWN', _, _, _, _} ->
                FnOnTerminate(Reason)
        end
    end).
