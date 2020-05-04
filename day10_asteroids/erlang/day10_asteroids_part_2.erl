-module(day10_asteroids_part_2).

-include_lib("eunit/include/eunit.hrl").

-export([blast_asteroids/1]).

% Offset from one asteroid to another.
-record(vector, {
    dx,
    dy
}).

-record(point, {
    x,
    y
}).

% List of asteroids, plus the vector matrix between them.
% Direction matrix = list with one entry per asteroid A, each row = list with one entry per asterioid B,
% each entry = vector from asteroid A to asteroid B.  So, this is an antisymmetric matrix.
-record(asteroid_matrix, {
    asteroid_list,
    matrix
}).


blast_asteroids(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    % AsteroidMap = list, each row = list, each entry = '.' if no asteroid, '#' if asteroid
    AsteroidMap = try load_map(Device)
      after file:close(Device)
    end,
    % Matrix = list with one entry per asteroid A, each row = list with one entry per asterioid B,
    % each entry = vector from asteroid A to asteroid B.  So, this is an antisymmetric matrix.
    Matrix = calc_matrix(0, AsteroidMap, #asteroid_matrix{asteroid_list = [], matrix = []}),
    % Best monitoring point corresponds to the row with the most distinct directions.
    {StationLocation, NumberOfVisibleAsteroids, AsteroidVectors} = identify_best_monitoring_point(Matrix),
    io:fwrite("Blasting asteroids from ~w from where ~w asteroids are visible.~n", [StationLocation, NumberOfVisibleAsteroids]),
    AsteroidsByClock = sort_targets(Matrix#asteroid_matrix.asteroid_list, AsteroidVectors),
    blast_asteroids_in_clock_order(AsteroidsByClock).


blast_asteroids_in_clock_order([{P, V}|Rest]) ->
    blast_asteroid(1, P),
    blast_next_asteroid(2, V, Rest, []).


blast_next_asteroid(N, LastV, [{P, V} = A|Rest], DeferredTargets) ->
    case compare_angle(LastV, V) of
        same ->
            blast_next_asteroid(N, LastV, Rest, DeferredTargets ++ [A]);
        _ ->
            blast_asteroid(N, P),
            blast_next_asteroid(N + 1, V, Rest, DeferredTargets)
    end;
blast_next_asteroid(N, _, [], [{P, V}|RestDeferredTargets]) ->
    blast_asteroid(N, P),
    blast_next_asteroid(N + 1, V, RestDeferredTargets, []);
blast_next_asteroid(_, _, [], []) ->
    {}.


blast_asteroid(N, P) ->
    io:fwrite("Blasted asteroid #~w at ~w~n", [N, P]).


sort_targets(AsteroidPositions, AsteroidVectors) ->
    lists:sort(fun(A, B) -> relative_asteroid_lt(A, B) end, 
        merge_positions_and_vectors(AsteroidPositions, AsteroidVectors)).


merge_positions_and_vectors([_|PRest], [self|VRest]) ->
    merge_positions_and_vectors(PRest, VRest);
merge_positions_and_vectors([P1|PRest], [V1|VRest]) ->
    [{P1, V1}|merge_positions_and_vectors(PRest, VRest)];
merge_positions_and_vectors([], []) ->
    [].


calc_matrix(Y, [AsteroidRow|Rest], MatrixSoFar) ->
    calc_matrix(Y + 1, Rest, calc_matrix_for_row_of_asteroids(0, Y, AsteroidRow, MatrixSoFar));
calc_matrix(_, [], MatrixSoFar) ->
    MatrixSoFar.


calc_matrix_for_row_of_asteroids(X, Y, [$.|Rest], MatrixSoFar) ->
    calc_matrix_for_row_of_asteroids(X + 1, Y, Rest, MatrixSoFar);
calc_matrix_for_row_of_asteroids(X, Y, [$#|Rest], MatrixSoFar) ->
    NewMatrix = append_asteroid(#point{x = X, y = Y}, MatrixSoFar),
    calc_matrix_for_row_of_asteroids(X + 1, Y, Rest, NewMatrix);
calc_matrix_for_row_of_asteroids(_, _, [], MatrixSoFar) ->
    MatrixSoFar.


append_asteroid(P, #asteroid_matrix{asteroid_list = AsteroidListSoFar, matrix = AsteroidMatrixSoFar}) ->
    VectorsFromPToOtherAsteroids = calc_vectors(P, AsteroidListSoFar),
    MatrixWithNewColumn = add_reversed_vector_column(AsteroidMatrixSoFar, VectorsFromPToOtherAsteroids),
    #asteroid_matrix{asteroid_list = AsteroidListSoFar ++ [P], 
        matrix = MatrixWithNewColumn ++ [VectorsFromPToOtherAsteroids ++ [self]]}.


calc_vectors(A, [B|Rest]) ->
    [calc_vector(A, B)|calc_vectors(A, Rest)];
calc_vectors(_, []) ->
    [].


calc_vector(P, P) ->
    self;
calc_vector(#point{x = FromX, y = FromY}, #point{x = ToX, y = ToY}) ->
    #vector{dx = ToX - FromX, dy = ToY - FromY}.


square_distance(X, Y) when (X > 0) and (Y > 0) ->
    X + Y;
square_distance(X, Y) when (X > 0) ->
    X - Y;
square_distance(X, Y) when (Y > 0) ->
    Y - X;
square_distance(X, Y) ->
    -X - Y.


relative_asteroid_lt(self, _) ->
    false;
relative_asteroid_lt(_, self) ->
    true;
relative_asteroid_lt({_, D1}, {_, D2}) ->
    vector_lt(D1, D2).


% Compare vectors by clock angle (first), then distance shortest to largest.
vector_lt(#vector{dx = DX1, dy = DY1} = D1, #vector{dx = DX2, dy = DY2} = D2) ->
    case compare_angle(D1, D2) of
        less ->
            true;
        greater ->
            false;
        same ->
            L1 = square_distance(DX1, DY1),
            L2 = square_distance(DX2, DY2),
            if
                L1 < L2 ->
                    true;
                true ->
                    false
            end
    end.


compare_angle(#vector{dx = DX1, dy = DY1} = D1, #vector{dx = DX2, dy = DY2} = D2) ->
    Q1 = quadrant(D1),
    Q2 = quadrant(D2),
    if
        Q1 < Q2 ->
            less;
        Q1 > Q2 ->
            greater;
        true ->
            SlopeDifference = DX2 * DY1 - DY2 * DX1,
            if
                SlopeDifference < 0 ->
                    less;
                SlopeDifference > 0 ->
                    greater;
                true ->
                    same
            end
    end.


quadrant(#vector{dx = DX1, dy = DY1}) when (DX1 >= 0) and (DY1 =< 0) ->
    0;
quadrant(#vector{dx = DX1}) when DX1 >= 0 ->
    1;
quadrant(#vector{dy = DY1}) when DY1 > 0 ->
    2;
quadrant(_) ->
    3.


add_reversed_vector_column([TopRow|RestRows], [#vector{dx = DX, dy = DY}|RestDirections]) ->
    ReversedVector = #vector{dx = -DX, dy = -DY},
    [TopRow ++ [ReversedVector]|add_reversed_vector_column(RestRows, RestDirections)];
add_reversed_vector_column([], []) ->
    [].


identify_best_monitoring_point(#asteroid_matrix{asteroid_list = [FirstPoint|OtherPoints], matrix = [FirstRow|OtherRows]}) ->
    FirstPointVisibleAsteroids = count_distinct_directions(FirstRow),
    identify_better_monitoring_point(#asteroid_matrix{asteroid_list = OtherPoints, matrix = OtherRows}, 
        {FirstPoint, FirstPointVisibleAsteroids, FirstRow}).


identify_better_monitoring_point(#asteroid_matrix{asteroid_list = [FirstPoint|OtherPoints], matrix = [FirstRow|OtherRows]}, 
    {BestPointSoFar, BestPointVisibility, AsteroidVectors}) ->
    FirstPointVisibleAsteroids = count_distinct_directions(FirstRow),
    if 
        FirstPointVisibleAsteroids > BestPointVisibility ->
            identify_better_monitoring_point(#asteroid_matrix{asteroid_list = OtherPoints, matrix = OtherRows}, 
                {FirstPoint, FirstPointVisibleAsteroids, FirstRow});
        true ->
            identify_better_monitoring_point(#asteroid_matrix{asteroid_list = OtherPoints, matrix = OtherRows}, 
                {BestPointSoFar, BestPointVisibility, AsteroidVectors})
    end;
identify_better_monitoring_point(#asteroid_matrix{asteroid_list = [], matrix = []}, 
    {BestPointSoFar, BestPointVisibility, AsteroidVectors}) ->
    {BestPointSoFar, BestPointVisibility, AsteroidVectors}.


count_distinct_directions(L) ->
    length(list_distinct_directions(L, [])).


list_distinct_directions([self|Rest], DistinctDirections) ->
    list_distinct_directions(Rest, DistinctDirections);
list_distinct_directions([A|Rest], DistinctDirections) ->
    case is_distinct_direction(A, DistinctDirections) of
        true ->
            list_distinct_directions(Rest, [A|DistinctDirections]);
        _ -> 
            list_distinct_directions(Rest, DistinctDirections)
    end;
list_distinct_directions([], DistinctDirections) ->
    DistinctDirections.


is_distinct_direction(D, [D1|Rest]) ->
    case compare_angle(D, D1) of
        same -> false;
        _ -> is_distinct_direction(D, Rest)
    end;
is_distinct_direction(_, []) ->
    true.


parse_line(LineSoFar, "\n") ->
    LineSoFar;
parse_line(LineSoFar, "\r") ->
    LineSoFar;
parse_line(LineSoFar, [A|Rest]) ->
    parse_line(LineSoFar ++ [A], Rest).


load_map(Device) ->
    case io:get_line(Device, "") of
        eof  -> 
            [];
        Line -> 
            [parse_line("", Line)|load_map(Device)]
    end.


quadrant_test() ->
    0 = quadrant(#vector{dx = 2, dy = -1}),
    1 = quadrant(#vector{dx = 2, dy = 10}),
    2 = quadrant(#vector{dx = -5, dy = 7}),
    3 = quadrant(#vector{dx = -5, dy = -7}).


compare_angle_test() ->
    same = compare_angle(#vector{dx = 0, dy = 1}, #vector{dx = 0, dy = 1}),
    same = compare_angle(#vector{dx = 0, dy = 1}, #vector{dx = 0, dy = 5}),
    same = compare_angle(#vector{dx = 4, dy = 1}, #vector{dx = 8, dy = 2}),
    less = compare_angle(#vector{dx = 4, dy = -1}, #vector{dx = 8, dy = 2}).


is_distinct_direction_test() ->
    false = is_distinct_direction(#vector{dx = 4, dy = -1},
        [#vector{dx = 44, dy = -1}, #vector{dx = 8, dy = -2}]),
    true = is_distinct_direction(#vector{dx = 1, dy = -1},
        [#vector{dx = 44, dy = -1}, #vector{dx = 8, dy = -2}]).
