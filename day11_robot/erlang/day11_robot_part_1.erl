-module(day11_robot_part_1).

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
    map_size(do_paint_hull(Computer, #robot{x=0, y=0, xd=0, yd=-1}, #{})).


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