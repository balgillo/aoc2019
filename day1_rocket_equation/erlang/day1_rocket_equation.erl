-module(day1_rocket_equation).

-export([calc_fuel/1]).

calc_fuel(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    Masses = try load_masses(Device, [])
      after file:close(Device)
    end,
    calc_total_fuel(Masses, 0).


calc_total_fuel([M|Rest], T) ->
    calc_total_fuel(Rest, T + (M div 3) - 2);
calc_total_fuel([], T) ->
    T.


trim_lf(C, [A|Rest]) when Rest =:= "\n" ->
    C ++ [A];
trim_lf(C, [A|Rest]) ->
    trim_lf(C ++ [A], Rest).


parse_line(L) ->
    list_to_integer(trim_lf("", L)).


load_masses(Device, Masses) ->
    case io:get_line(Device, "") of
        eof  -> Masses;
        Line -> 
            Mass = parse_line(Line),
            load_masses(Device, Masses ++ [Mass])
    end.
