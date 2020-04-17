-module(day5_test).

-export([run/1]).


run(P) ->
    exec(P, 0).


read(P, I, M) ->
    Index = lists:nth(I + 1, P),
    case M of
        0 -> lists:nth(Index + 1, P);
        1 -> Index
    end.


exec(P, I) ->
    C = lists:nth(I + 1, P),
    case C rem 100 of
        1 ->
            ResultIndex = lists:nth(I + 4, P),
            NP = lists:sublist(P, ResultIndex) ++ [read(P, I + 1, (C div 100) rem 10) + read(P, I + 2, (C div 1000) rem 10)] ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        2 ->
            ResultIndex = lists:nth(I + 4, P),
            NP = lists:sublist(P, ResultIndex) ++ [read(P, I + 1, (C div 100) rem 10) * read(P, I + 2, (C div 1000) rem 10)] ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        3 ->
            ResultIndex = lists:nth(I + 2, P),
            case io:fread("Input: ", "~d") of
                {ok, [InputValue]} ->
                    NP = lists:sublist(P, ResultIndex) ++ [InputValue] ++ lists:nthtail(ResultIndex + 1, P),
                    exec(NP, I + 2);
                _ -> fail
            end;
        4 ->
            io:fwrite("~s~w~n", ["Output: ", read(P, I + 1, (C div 100) rem 10)]),
            exec(P, I + 2);
        99 ->
            P
    end.


