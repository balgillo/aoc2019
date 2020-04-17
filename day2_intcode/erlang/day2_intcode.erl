-module(day2_intcode).

-export([run/1]).


run(P) ->
    exec(P, 0).


read(P, I) ->
    Index = lists:nth(I + 1, P),
    lists:nth(Index + 1, P).


exec(P, I) ->
    case lists:nth(I + 1, P) of
        1 ->
            ResultIndex = lists:nth(I + 4, P),
            NP = lists:sublist(P, ResultIndex) ++ [read(P, I + 1) + read(P, I + 2)] ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        2 ->
            ResultIndex = lists:nth(I + 4, P),
            NP = lists:sublist(P, ResultIndex) ++ [read(P, I + 1) * read(P, I + 2)]  ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        99 ->
            P
    end.


