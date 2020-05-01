-module(day10_asteroids_part_1).

-export([find_best_monitoring_point/1]).

% Represents a direction via its slope.  It uniquely corresponds to a value in the
% affinely-extended rational number line (Q u {+infinity, -infinity} where infinity = up
% and -infinity = down).  To achieve this, dx and dy must be coprime.
-record(direction, {
    dx,
    dy
}).

-record(point, {
    x,
    y
}).

% List of asteroids, plus the direction matrix between them.
% Direction matrix = list with one entry per asteroid A, each row = list with one entry per asterioid B,
% each entry = direction from asteroid A to asteroid B.  So, this is an antisymmetric matrix.
-record(direction_matrix, {
    asteroid_list,
    matrix
}).


find_best_monitoring_point(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    % AsteroidMap = list, each row = list, each entry = '.' if no asteroid, '#' if asteroid
    AsteroidMap = try load_map(Device)
      after file:close(Device)
    end,
    % Direction matrix = list with one entry per asteroid A, each row = list with one entry per asterioid B,
    % each entry = direction from asteroid A to asteroid B.  So, this is an antisymmetric matrix.
    DirectionMatrix = calc_direction_matrix(0, AsteroidMap, #direction_matrix{asteroid_list = [], matrix = []}),
    % Best monitoring point corresponds to the row with the most distinct directions.
    identify_best_monitoring_point(DirectionMatrix).


calc_direction_matrix(Y, [AsteroidRow|Rest], MatrixSoFar) ->
    calc_direction_matrix(Y + 1, Rest, calc_direction_matrix_for_row_of_asteroids(0, Y, AsteroidRow, MatrixSoFar));
calc_direction_matrix(_, [], MatrixSoFar) ->
    MatrixSoFar.


calc_direction_matrix_for_row_of_asteroids(X, Y, [$.|Rest], MatrixSoFar) ->
    calc_direction_matrix_for_row_of_asteroids(X + 1, Y, Rest, MatrixSoFar);
calc_direction_matrix_for_row_of_asteroids(X, Y, [$#|Rest], MatrixSoFar) ->
    NewMatrix = append_asteroid(#point{x = X, y = Y}, MatrixSoFar),
    calc_direction_matrix_for_row_of_asteroids(X + 1, Y, Rest, NewMatrix);
calc_direction_matrix_for_row_of_asteroids(_, _, [], MatrixSoFar) ->
    MatrixSoFar.


append_asteroid(P, #direction_matrix{asteroid_list = AsteroidListSoFar, matrix = AsteroidMatrixSoFar}) ->
    DirectionsFromPToOtherAsteroids = calc_directions(P, AsteroidListSoFar),
    MatrixWithNewColumn = add_reversed_direction_column(AsteroidMatrixSoFar, DirectionsFromPToOtherAsteroids),
    #direction_matrix{asteroid_list = AsteroidListSoFar ++ [P], 
        matrix = MatrixWithNewColumn ++ [DirectionsFromPToOtherAsteroids ++ [self]]}.


calc_directions(A, [B|Rest]) ->
    [calc_direction(A, B)|calc_directions(A, Rest)];
calc_directions(_, []) ->
    [].


calc_direction(P, P) ->
    self;
calc_direction(#point{x = FromX, y = FromY}, #point{x = ToX, y = ToY}) ->
    DX = ToX - FromX,
    DY = ToY - FromY,
    G = gcd(DX, DY),
    #direction{dx = DX / G, dy = DY / G}.


add_reversed_direction_column([TopRow|RestRows], [#direction{dx = DX, dy = DY}|RestDirections]) ->
    [TopRow ++ [#direction{dx = -DX, dy = -DY}]|add_reversed_direction_column(RestRows, RestDirections)];
add_reversed_direction_column([], []) ->
    [].


% Euclidean algorithm, but allowing for negative and zeroes
gcd(A, B) when A < 0 ->
    gcd(-A, B);
gcd(A, B) when B < 0 ->
    gcd(A, -B);
gcd(0, 0) ->
    0;
gcd(0, B) ->
    B;
gcd(A, 0) ->
    A;
gcd(A, B) when B > A ->
    gcd(B, A);
gcd(A, B) ->
    R = A rem B,
    case R of
        0 -> B;
        _ -> gcd(B, R)
    end.


identify_best_monitoring_point(#direction_matrix{asteroid_list = [FirstPoint|OtherPoints], matrix = [FirstRow|OtherRows]}) ->
    FirstPointVisibleAsteroids = count_distinct_directions(FirstRow),
    identify_better_monitoring_point(#direction_matrix{asteroid_list = OtherPoints, matrix = OtherRows}, {FirstPoint, FirstPointVisibleAsteroids}).


identify_better_monitoring_point(#direction_matrix{asteroid_list = [FirstPoint|OtherPoints], matrix = [FirstRow|OtherRows]}, {BestPointSoFar, BestPointVisibility}) ->
    FirstPointVisibleAsteroids = count_distinct_directions(FirstRow),
    if 
        FirstPointVisibleAsteroids > BestPointVisibility ->
            identify_better_monitoring_point(#direction_matrix{asteroid_list = OtherPoints, matrix = OtherRows}, {FirstPoint, FirstPointVisibleAsteroids});
        true ->
            identify_better_monitoring_point(#direction_matrix{asteroid_list = OtherPoints, matrix = OtherRows}, {BestPointSoFar, BestPointVisibility})
    end;
identify_better_monitoring_point(#direction_matrix{asteroid_list = [], matrix = []}, {BestPointSoFar, BestPointVisibility}) ->
    {BestPointSoFar, BestPointVisibility}.


count_distinct_directions(L) ->
    length(list_distinct_directions(L, [])).


list_distinct_directions([self|Rest], DistinctDirections) ->
    list_distinct_directions(Rest, DistinctDirections);
list_distinct_directions([A|Rest], DistinctDirections) ->
    case lists:member(A, DistinctDirections) of
        true ->
            list_distinct_directions(Rest, DistinctDirections);
        _ -> 
            list_distinct_directions(Rest, [A|DistinctDirections])
    end;
list_distinct_directions([], DistinctDirections) ->
    DistinctDirections.


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
