-module(day3_crossed_wires).

-export([find_closest_crossing/1]).

find_closest_crossing(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    [Wire1, Wire2] = try load_wire_moves(Device, [])
        after file:close(Device)
    end,
    find_crossing(none, Wire1, 0, 0, Wire2).


closest(none, B) -> 
    B;
closest(A, none) -> 
    A;
closest(A, B) when A < B -> 
    A;
closest(_, B) -> 
    B.


manhattan_distance(X, Y) when (X < 0) and (Y < 0) ->
    -X - Y;
manhattan_distance(X, Y) when (X < 0) ->
    -X + Y;
manhattan_distance(X, Y) when (Y < 0) ->
    X - Y;
manhattan_distance(X, Y) ->
    X + Y.


find_crossing(BestDistance, [[Dir|StepsString]|Rest], X, Y, Wire2) ->
    Steps = list_to_integer(StepsString),
    {NewCrossing, NewX, NewY} = case Dir of
        $U -> 
            {find_crossing_of_vertical_wire(BestDistance, X, Y - Steps, Y, Wire2, 0, 0), X, Y - Steps};
        $D -> 
            {find_crossing_of_vertical_wire(BestDistance, X, Y, Y + Steps, Wire2, 0, 0), X, Y + Steps};
        $L -> 
            {find_crossing_of_horizontal_wire(BestDistance, X - Steps, Y, X, Wire2, 0, 0), X - Steps, Y};
        $R -> 
            {find_crossing_of_horizontal_wire(BestDistance, X, Y, X + Steps, Wire2, 0, 0), X + Steps, Y}
    end,
    find_crossing(closest(NewCrossing, BestDistance), Rest, NewX, NewY, Wire2);
find_crossing(BestDistance, [], _, _, _) ->
    BestDistance.


find_crossing_of_vertical_wire(BestDistance, AX, AY1, AY2, [[Dir|StepsString]|Rest], BX, BY) ->
    Steps = list_to_integer(StepsString),
    NewCrossing = case Dir of
        $U -> 
            find_crossing_of_vertical_wire(BestDistance, AX, AY1, AY2, Rest, BX, BY - Steps);
        $D -> 
            find_crossing_of_vertical_wire(BestDistance, AX, AY1, AY2, Rest, BX, BY + Steps);
        $L -> 
            LaterCrossingDistance = find_crossing_of_vertical_wire(BestDistance, AX, AY1, AY2, Rest, BX - Steps, BY),
            if
                (AX >= BX - Steps) and (AX =< BX) and (BY >= AY1) and (BY =< AY2) and ((AX /= 0) or (BX /= 0)) ->
                    CrossingDistance = manhattan_distance(AX, BY),
                    closest(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $R -> 
            LaterCrossingDistance = find_crossing_of_vertical_wire(BestDistance, AX, AY1, AY2, Rest, BX + Steps, BY),
            if
                (AX >= BX) and (AX =< BX + Steps) and (BY >= AY1) and (BY =< AY2) and ((AX /= 0) or (BX /= 0)) ->
                    CrossingDistance = manhattan_distance(AX, BY),
                    closest(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end
    end,
    closest(NewCrossing, BestDistance);
find_crossing_of_vertical_wire(BestDistance, _, _, _, [], _, _) ->
    BestDistance.


find_crossing_of_horizontal_wire(BestDistance, AX1, AY, AX2, [[Dir|StepsString]|Rest], BX, BY) ->
    Steps = list_to_integer(StepsString),
    NewCrossing = case Dir of
        $U -> 
            LaterCrossingDistance = find_crossing_of_horizontal_wire(BestDistance, AX1, AY, AX2, Rest, BX, BY - Steps),
            if
                (AX1 =< BX) and (AX2 >= BX) and (BY - Steps =< AY) and (BY >= AY) and ((BX /= 0) or (AY /= 0)) ->
                    CrossingDistance = manhattan_distance(BX, AY),
                    closest(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $D -> 
            LaterCrossingDistance = find_crossing_of_horizontal_wire(BestDistance, AX1, AY, AX2, Rest, BX, BY + Steps),
            if
                (AX1 =< BX) and (AX2 >= BX) and (BY =< AY) and (BY + Steps >= AY) and ((BX /= 0) or (AY /= 0)) ->
                    CrossingDistance = manhattan_distance(BX, AY),
                    closest(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $L -> 
            find_crossing_of_horizontal_wire(BestDistance, AX1, AY, AX2, Rest, BX - Steps, BY);
        $R -> 
            find_crossing_of_horizontal_wire(BestDistance, AX1, AY, AX2, Rest, BX + Steps, BY)
    end,
    closest(NewCrossing, BestDistance);
find_crossing_of_horizontal_wire(BestDistance, _, _, _, [], _, _) ->
    BestDistance.


trim_lf(C, [A|Rest]) when Rest =:= "\n" ->
    C ++ [A];
trim_lf(C, [A|Rest]) ->
    trim_lf(C ++ [A], Rest).


parse_line(W, M, [A|Rest]) when [A] =:= "," ->
    parse_line(W ++ [M], "", Rest);
parse_line(W, M, [A|Rest]) ->
    parse_line(W, M ++ [A], Rest);
parse_line(W, M, []) ->
    W ++ [M].



% wires[0] = list of lists, being the moves in order e.g. "U1,D2" -> ["U1", "D2"]
% wires[1] = same but for second wire
load_wire_moves(Device, Wires) ->
    case io:get_line(Device, "") of
        eof  -> Wires;
        Line -> 
            WireMoves = parse_line([], "", trim_lf("", Line)),
            load_wire_moves(Device, Wires ++ [WireMoves])
    end.
