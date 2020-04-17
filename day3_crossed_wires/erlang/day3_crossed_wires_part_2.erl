-module(day3_crossed_wires_part_2).

-export([find_closest_crossing/1]).

find_closest_crossing(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    [Wire1, Wire2] = try load_wire_moves(Device, [])
        after file:close(Device)
    end,
    find_crossing(none, Wire1, 0, 0, 0, Wire2).


minimal(none, B) -> 
    B;
minimal(A, none) -> 
    A;
minimal(0, B) -> 
    B;
minimal(A, 0) -> 
    A;
minimal(A, B) when A < B -> 
    A;
minimal(_, B) -> 
    B.


find_crossing(BestDistance, [[Dir|StepsString]|Rest], Wire1X, Wire1Y, Wire1StepsSoFar, Wire2) ->
    SegmentSteps = list_to_integer(StepsString),
    {NewCrossingWire1TotalSteps, NewX, NewY} = case Dir of
        $U -> 
            {find_crossing_of_up_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, SegmentSteps, Wire2, 0, 0, 0),
            Wire1X, Wire1Y - SegmentSteps};
        $D -> 
            {find_crossing_of_down_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, SegmentSteps, Wire2, 0, 0, 0),
            Wire1X, Wire1Y + SegmentSteps};
        $L -> 
            {find_crossing_of_left_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, SegmentSteps, Wire2, 0, 0, 0),
            Wire1X - SegmentSteps, Wire1Y};
        $R -> 
            {find_crossing_of_right_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, SegmentSteps, Wire2, 0, 0, 0),
            Wire1X + SegmentSteps, Wire1Y}
    end,
    find_crossing(minimal(NewCrossingWire1TotalSteps, BestDistance), Rest, NewX, NewY, Wire1StepsSoFar + SegmentSteps, Wire2);
find_crossing(BestDistance, [], _, _, _, _) ->
    BestDistance.


find_crossing_of_up_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, [[Dir|StepsString]|Rest], Wire2X, Wire2Y, Wire2StepsSoFar) ->
    Wire2SegmentSteps = list_to_integer(StepsString),
    NewCrossing = case Dir of
        $U -> 
            find_crossing_of_up_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y - Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps);
        $D -> 
            find_crossing_of_up_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y + Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps);
        $L -> 
            LaterCrossingDistance = find_crossing_of_up_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X - Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire1X >= Wire2X - Wire2SegmentSteps) and (Wire1X =< Wire2X) and (Wire2Y >= Wire1Y - Wire1SegmentSteps) and (Wire2Y =< Wire1Y) ->
                    % crossing at Wire1X, Wire2Y, wire1 going up (needs Wire1Y - Wire2Y steps to intesection), wire2 going left (needs Wire2X - Wire1X steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire1Y - Wire2Y + Wire2StepsSoFar + Wire2X - Wire1X,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $R -> 
            LaterCrossingDistance = find_crossing_of_up_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X + Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire1X >= Wire2X) and (Wire1X =< Wire2X + Wire2SegmentSteps) and (Wire2Y >= Wire1Y - Wire1SegmentSteps) and (Wire2Y =< Wire1Y) ->
                    % crossing at Wire1X, Wire2Y, wire1 going up (needs Wire1Y - Wire2Y steps to intesection), wire2 going right (needs Wire1X - Wire2X steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire1Y - Wire2Y + Wire2StepsSoFar + Wire1X - Wire2X,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end
    end,
    minimal(NewCrossing, BestDistance);
find_crossing_of_up_segment(BestDistance, _, _, _, _, [], _, _, _) ->
    BestDistance.


find_crossing_of_down_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, [[Dir|StepsString]|Rest], Wire2X, Wire2Y, Wire2StepsSoFar) ->
    Wire2SegmentSteps = list_to_integer(StepsString),
    NewCrossing = case Dir of
        $U -> 
            find_crossing_of_down_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y - Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps);
        $D -> 
            find_crossing_of_down_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y + Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps);
        $L -> 
            LaterCrossingDistance = find_crossing_of_down_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X - Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire1X >= Wire2X - Wire2SegmentSteps) and (Wire1X =< Wire2X) and (Wire2Y >= Wire1Y) and (Wire2Y =< Wire1Y + Wire1SegmentSteps) ->
                    % crossing at Wire1X, Wire2Y, wire1 going down (needs Wire2Y - Wire1Y steps to intesection), wire2 going left (needs Wire2X - Wire1X steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire2Y - Wire1Y + Wire2StepsSoFar + Wire2X - Wire1X,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $R -> 
            LaterCrossingDistance = find_crossing_of_down_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X + Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire1X >= Wire2X) and (Wire1X =< Wire2X + Wire2SegmentSteps) and (Wire2Y >= Wire1Y) and (Wire2Y =< Wire1Y + Wire1SegmentSteps) ->
                    % crossing at Wire1X, Wire2Y, wire1 going down (needs Wire2Y - Wire1Y steps to intesection), wire2 going right (needs Wire1X - Wire2X steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire2Y - Wire1Y + Wire2StepsSoFar + Wire1X - Wire2X,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end
    end,
    minimal(NewCrossing, BestDistance);
find_crossing_of_down_segment(BestDistance, _, _, _, _, [], _, _, _) ->
    BestDistance.


find_crossing_of_left_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, [[Dir|StepsString]|Rest], Wire2X, Wire2Y, Wire2StepsSoFar) ->
    Wire2SegmentSteps = list_to_integer(StepsString),
    NewCrossing = case Dir of
        $U -> 
            LaterCrossingDistance = find_crossing_of_left_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y - Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire2X >= Wire1X - Wire1SegmentSteps) and (Wire2X =< Wire1X) and (Wire1Y >= Wire2Y - Wire2SegmentSteps) and (Wire1Y =< Wire2Y) ->
                    % crossing at Wire2X, Wire1Y, wire1 going left (needs Wire1X - Wire2X steps to intersection), wire2 going up (needs Wire2Y - Wire1Y steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire1X - Wire2X + Wire2StepsSoFar + Wire2Y - Wire1Y,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $D -> 
            LaterCrossingDistance = find_crossing_of_left_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y + Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire2X >= Wire1X - Wire1SegmentSteps) and (Wire2X =< Wire1X) and (Wire1Y >= Wire2Y) and (Wire1Y =< Wire2Y + Wire2SegmentSteps) ->
                    % crossing at Wire2X, Wire1Y, wire1 going left (needs Wire1X - Wire2X steps to intersection), wire2 going down (needs Wire1Y - Wire2Y steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire1X - Wire2X + Wire2StepsSoFar + Wire1Y - Wire2Y,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $L -> 
            find_crossing_of_left_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X - Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps);
        $R -> 
            find_crossing_of_left_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X + Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps)
    end,
    minimal(NewCrossing, BestDistance);
find_crossing_of_left_segment(BestDistance, _, _, _, _, [], _, _, _) ->
    BestDistance.


find_crossing_of_right_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, [[Dir|StepsString]|Rest], Wire2X, Wire2Y, Wire2StepsSoFar) ->
    Wire2SegmentSteps = list_to_integer(StepsString),
    NewCrossing = case Dir of
        $U -> 
            LaterCrossingDistance = find_crossing_of_right_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y - Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire2X >= Wire1X) and (Wire2X =< Wire1X + Wire1SegmentSteps) and (Wire1Y >= Wire2Y - Wire2SegmentSteps) and (Wire1Y =< Wire2Y) ->
                    % crossing at Wire2X, Wire1Y, wire1 going right (needs Wire2X - Wire1X steps to intersection), wire2 going up (needs Wire2Y - Wire1Y steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire2X - Wire1X + Wire2StepsSoFar + Wire2Y - Wire1Y,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $D -> 
            LaterCrossingDistance = find_crossing_of_right_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X, Wire2Y + Wire2SegmentSteps, Wire2StepsSoFar + Wire2SegmentSteps),
            if
                (Wire2X >= Wire1X) and (Wire2X =< Wire1X + Wire1SegmentSteps) and (Wire1Y >= Wire2Y) and (Wire1Y =< Wire2Y + Wire2SegmentSteps) ->
                    % crossing at Wire2X, Wire1Y, wire1 going right (needs Wire2X - Wire1X steps to intersection), wire2 going down (needs Wire1Y - Wire2Y steps to intersection)
                    CrossingDistance = Wire1StepsSoFar + Wire2X - Wire1X + Wire2StepsSoFar + Wire1Y - Wire2Y,
                    minimal(CrossingDistance, LaterCrossingDistance);
                true -> LaterCrossingDistance
            end;
        $L -> 
            find_crossing_of_right_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X - Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps);
        $R -> 
            find_crossing_of_right_segment(BestDistance, Wire1X, Wire1Y, Wire1StepsSoFar, Wire1SegmentSteps, Rest, Wire2X + Wire2SegmentSteps, Wire2Y, Wire2StepsSoFar + Wire2SegmentSteps)
    end,
    minimal(NewCrossing, BestDistance);
find_crossing_of_right_segment(BestDistance, _, _, _, _, [], _, _, _) ->
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
