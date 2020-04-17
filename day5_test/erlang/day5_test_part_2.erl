-module(day5_test_part_2).

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
        1 -> % add
            ResultIndex = lists:nth(I + 4, P),
            A = read(P, I + 1, (C div 100) rem 10),
            B = read(P, I + 2, (C div 1000) rem 10),
            NP = lists:sublist(P, ResultIndex) ++ [A + B] ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        2 -> % mul
            ResultIndex = lists:nth(I + 4, P),
            A = read(P, I + 1, (C div 100) rem 10),
            B = read(P, I + 2, (C div 1000) rem 10),
            NP = lists:sublist(P, ResultIndex) ++ [A * B] ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        3 -> % input
            ResultIndex = lists:nth(I + 2, P),
            case io:fread("Input: ", "~d") of
                {ok, [InputValue]} ->
                    NP = lists:sublist(P, ResultIndex) ++ [InputValue] ++ lists:nthtail(ResultIndex + 1, P),
                    exec(NP, I + 2);
                _ -> fail
            end;
        4 -> % output
            A = read(P, I + 1, (C div 100) rem 10),
            io:fwrite("~s~w~n", ["Output: ", A]),
            exec(P, I + 2);
        5 -> % jump if true
            A = read(P, I + 1, (C div 100) rem 10),
            B = read(P, I + 2, (C div 1000) rem 10),
            case A of
                0 -> exec(P, I + 3);
                _ -> exec(P, B)
            end;
        6 -> % jump if false
            A = read(P, I + 1, (C div 100) rem 10),
            B = read(P, I + 2, (C div 1000) rem 10),
            case A of
                0 -> exec(P, B);
                _ -> exec(P, I + 3)
            end;
        7 -> % less than
            ResultIndex = lists:nth(I + 4, P),
            A = read(P, I + 1, (C div 100) rem 10),
            B = read(P, I + 2, (C div 1000) rem 10),
            R = if A < B -> 1; true -> 0 end,
            NP = lists:sublist(P, ResultIndex) ++ [R] ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        8 -> % equals
            ResultIndex = lists:nth(I + 4, P),
            A = read(P, I + 1, (C div 100) rem 10),
            B = read(P, I + 2, (C div 1000) rem 10),
            R = if A =:= B -> 1; true -> 0 end,
            NP = lists:sublist(P, ResultIndex) ++ [R] ++ lists:nthtail(ResultIndex + 1, P),
            exec(NP, I + 4);
        99 -> % halt
            P
    end.


