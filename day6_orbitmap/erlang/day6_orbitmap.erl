-module(day6_orbitmap).

-export([calc_checksum/1]).

% Can only have maps #{ a => b } in newer versions of erlang.  So this does it with lists.

calc_checksum(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    Graph = try load_graph(Device, [])
      after file:close(Device)
    end,
    count_orbits(Graph, Graph).


count_orbits([], _) ->
    0;
count_orbits([{S1, _}|Rest], Graph) ->
    count_planet_orbits(S1, Graph, Graph) + count_orbits(Rest, Graph).


count_planet_orbits(S, [{S, C2}|_], Graph) ->
    1 + count_planet_orbits(C2, Graph, Graph);
count_planet_orbits(S1, [_|Rest], Graph) ->
    count_planet_orbits(S1, Rest, Graph);
count_planet_orbits(_, [], _) ->
    0.


trim_lf(C, [A|"\n"]) ->
    C ++ [A];
trim_lf(C, [A|Rest]) ->
    trim_lf(C ++ [A], Rest).


parse_line(C, [$)|Rest]) ->
    {C, trim_lf("", Rest)};
parse_line(C, [A|Rest]) ->
    parse_line(C ++ [A], Rest).


load_graph(Device, Graph) ->
    case io:get_line(Device, "") of
        eof  -> Graph;
        Line -> 
            {Centre, Satellite} = parse_line("", Line),
            load_graph(Device, Graph ++ [{Satellite, Centre}])
    end.
