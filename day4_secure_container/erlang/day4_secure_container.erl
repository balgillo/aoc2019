-module(day4_secure_container).

-export([count_codes/2]).

-spec count_codes(integer(), integer()) -> integer().
count_codes(MinInclusive, MaxInclusive) when MaxInclusive < MinInclusive ->
    0;
count_codes(MinInclusive, MaxInclusive) when MaxInclusive == MinInclusive ->
    count_if_valid(MinInclusive);
count_codes(MinInclusive, MaxInclusive) ->
    count_if_valid(MinInclusive) + count_codes(MinInclusive + 1, MaxInclusive).

count_if_valid(N) ->
    NS = integer_to_list(N),
    case non_decreasing_with_run(NS, false) of
        false -> 
            0;
        true -> 
            1
    end.

non_decreasing_with_run([_], RunFound) ->
    RunFound;
non_decreasing_with_run([A,B|_], _) when A > B ->
    false;
non_decreasing_with_run([A,A|Rest], _) ->
    non_decreasing_with_run([A|Rest], true);
non_decreasing_with_run([_|Rest], RunFound) ->
    non_decreasing_with_run(Rest, RunFound).
