-module(day6_orbitmap_part_2).
-include_lib("eunit/include/eunit.hrl").

-export([count_orbital_transfers_from_you_to_santa/1]).

% Can only have maps #{ a => b } in newer versions of erlang.  So this does it with lists.

count_orbital_transfers_from_you_to_santa(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    Graph = try load_graph(Device, [])
      after file:close(Device)
    end,
    YouPath = get_path_to_centre("YOU", Graph),
    SantaPath = get_path_to_centre("SAN", Graph),
    count_transfers(YouPath, SantaPath).


get_path_to_centre(N, Graph) ->
    P = get_orbiting(N, Graph),
    case P of
        none -> [];
        _ -> [P] ++ get_path_to_centre(P, Graph)
    end.


get_orbiting(P, [{P, C}|_]) ->
    C;
get_orbiting(P, [_|Rest]) ->
    get_orbiting(P, Rest);
get_orbiting(_, []) ->
    none.


index_of(X, [X|_]) ->
    0;
index_of(X, [_|Rest]) ->
    case index_of(X, Rest) of
        none -> none;
        N -> 1 + N
    end;
index_of(_, []) ->
    none.


count_transfers([A|RestA], BL) ->
    case index_of(A, BL) of
        none -> 1 + count_transfers(RestA, BL);
        N -> N
    end.


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


count_transfers_test() ->
    1 = count_transfers(["1", "2", "3"], ["2"]),
    1 = count_transfers(["2"], ["1", "2", "3"]),
    2 = count_transfers(["4", "2", "3"], ["1", "2", "3"]).


get_orbiting_test() ->
    "y" = get_orbiting("x", [{"x", "y"}]),
    none = get_orbiting("x", [{"r", "f"}]).
