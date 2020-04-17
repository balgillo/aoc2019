-module(day4_secure_container_part_2).

-export([count_codes/2]).

-spec count_codes(integer(), integer()) -> integer().
count_codes(MinInclusive, MaxInclusive) when MaxInclusive < MinInclusive ->
    0;
count_codes(MinInclusive, MaxInclusive) when MaxInclusive =:= MinInclusive ->
    count_if_valid(MinInclusive);
count_codes(MinInclusive, MaxInclusive) ->
    count_if_valid(MinInclusive) + count_codes(MinInclusive + 1, MaxInclusive).

count_if_valid(N) ->
    NS = integer_to_list(N),
    case non_decreasing_with_run_of_2(NS, false) of
        false -> 
            0;
        true -> 
            1
    end.

skip_to_last(_, []) ->
    [];
skip_to_last(A, [A|Rest]) ->
    skip_to_last(A, Rest);
skip_to_last(A, L) ->
    [A|L].


% Strip the first character repeatedly.
% If the first three characters are the same, skip until there's only one left at the start
% (so we can still detect decreasing character after the run).
non_decreasing_with_run_of_2([], RunFound) ->
    RunFound;
non_decreasing_with_run_of_2([_], RunFound) ->
    RunFound;
non_decreasing_with_run_of_2([A,B|_], _) when A > B ->
    false;
non_decreasing_with_run_of_2([A,A,A|Rest], RunFound) ->
    non_decreasing_with_run_of_2(skip_to_last(A, Rest), RunFound);
non_decreasing_with_run_of_2([A,A|Rest], _) ->
    non_decreasing_with_run_of_2(skip_to_last(A, Rest), true);
non_decreasing_with_run_of_2([_|Rest], RunFound) ->
    non_decreasing_with_run_of_2(Rest, RunFound).
