-module(day11_robot_part_2).

-import(computer, [load_program/1, set_inputs/2, exec/1, get_outputs/1, clear_outputs/1, get_state/1]).
-export([paint_hull/0]).


-record(robot, {
    x,
    y,
    xd,
    yd
}).


paint_hull() ->
    {ok, Device} = file:open("../data/puzzle_input.csv", [read]),
    Computer = try computer:load_program(Device)
      after file:close(Device)
    end,
    Hull = do_paint_hull(Computer, #robot{x=0, y=0, xd=0, yd=-1}, 
        paint_panel(#{}, 0, 0, 1)),
    print_hull_panel_pattern(Hull).


get_panel_colour(Hull, X, Y) ->
    try maps:get({X, Y}, Hull)
    catch 
      error:_ -> 0
    end.


paint_panel(Hull, X, Y, Colour) ->
    maps:put({X, Y}, Colour, Hull).


do_paint_hull(Computer, #robot{x=X, y=Y, xd=XD, yd=YD}, Hull) ->
    Colour = get_panel_colour(Hull, X, Y),
    NewComputer = computer:exec(computer:set_inputs(Computer, [Colour])),
    case computer:get_state(NewComputer) of
        halted -> 
            Hull;
        _ ->
            [NewColour, Turn] = computer:get_outputs(NewComputer),
            NewHull = paint_panel(Hull, X, Y, NewColour),
            NewRobot = case Turn of
                0 -> #robot{x=X + YD, y=Y - XD, xd=YD, yd=-XD};
                1 -> #robot{x=X - YD, y=Y + XD, xd=-YD, yd=XD}
            end,
            do_paint_hull(computer:clear_outputs(NewComputer), NewRobot, NewHull)
    end.


print_hull_panel_pattern(Hull) ->
    {_, _, MinY, _} = Extent = find_pattern_extent({0, 0, 0, 0}, maps:to_list(Hull)),
    print_next_hull_row(MinY, Extent, Hull).


find_pattern_extent({MinX, MaxX, MinY, MaxY}, [{{X, Y}, _}|Rest]) ->
    find_pattern_extent({
        if X < MinX -> X; true -> MinX end,
        if X > MaxX -> X; true -> MaxX end,
        if Y < MinY -> Y; true -> MinY end,
        if Y > MaxY -> Y; true -> MaxY end
    }, Rest);
find_pattern_extent(Extent, []) ->
    Extent.


print_next_hull_row(Y, {MinX, MaxX, _, MaxY} = Extent, Hull) ->
    print_hull_row(MinX, Y, MaxX, Hull),
    io:fwrite("~n"),
    if 
        Y < MaxY ->
            print_next_hull_row(Y + 1, Extent, Hull);
        true ->
            {}
    end.


print_hull_row(X, Y, MaxX, Hull) ->
    io:fwrite("~s", [
        case get_panel_colour(Hull, X, Y) of
            0 -> "  ";
            1 -> "XX"
        end
    ]),
    if 
        X < MaxX -> 
            print_hull_row(X + 1, Y, MaxX, Hull);
        true ->
            {}
    end.