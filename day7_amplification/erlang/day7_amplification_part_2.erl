-module(day7_amplification_part_2).

-include_lib("eunit/include/eunit.hrl").

-export([max_amp_series_output/1]).


max_amp_series_output(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    InitMem = try load_program(Device)
      after file:close(Device)
    end,
    calc_max_amp_series_output(InitMem, [9, 8, 7, 6, 5], 0).


calc_max_amp_series_output(InitMem, AmpPhases, HighestOutput) ->
    AmpOutput = calc_amp_series_output(InitMem, AmpPhases),
    NewHighestOutput = 
        if 
            AmpOutput > HighestOutput -> 
                io:fwrite("~w improved output to ~w~n", [AmpPhases, AmpOutput]),
                AmpOutput;
            true -> HighestOutput
        end,
    NewAmpPhases = next_phase_sequence(AmpPhases),
    case NewAmpPhases of
        none -> NewHighestOutput;
        _ -> calc_max_amp_series_output(InitMem, NewAmpPhases, NewHighestOutput)
    end.


% Steinhaus–Johnson–Trotter algorithm
next_phase_sequence([A, B]) when A > B ->
    [B, A];
next_phase_sequence([_, _]) ->
    none;
next_phase_sequence(L) ->
    {BeforeMax, Max, AfterMax} = split_max(L),
    case AfterMax of
        [] -> 
            NextExcludingMax = next_phase_sequence(BeforeMax ++ AfterMax),
            case NextExcludingMax of
                none -> none;
                _ -> [Max] ++ NextExcludingMax
            end;
        [A|Rest] ->
            BeforeMax ++ [A, Max] ++ Rest
    end.


split_max([A|Rest]) when Rest =/= [] ->
    {BeforeMax, Max, AfterMax} = split_max(Rest),
    if
        A > Max ->
            {[], A, Rest};
        true ->
            {[A|BeforeMax], Max, AfterMax}
    end;
split_max([A]) ->
    {[], A, []}.


calc_amp_series_output(InitMem, AmpPhases) ->
    Computers = init_computers(InitMem, AmpPhases, 0),
    run_computers(Computers).


init_computers(InitMem, [AmpPhase|Rest], PreviousAmpOutput) ->
    {Mem, IP, State, [AmpOutput]} = exec(InitMem, 0, [AmpPhase, PreviousAmpOutput], []),
    Pos = case Rest of
        [] -> last;
        _ -> notlast
    end,
    [{Pos, Mem, IP, State, [AmpOutput]}] ++ init_computers(InitMem, Rest, AmpOutput);
init_computers(_, [], _) ->
    [].


run_computers([{last, _, _, halted, [AmpOutput]}|_]) ->
    AmpOutput;
run_computers([{Pos, Mem, IP, halted, [AmpOutput]}|Rest]) ->
    run_computers(Rest ++ [{Pos, Mem, IP, halted, [AmpOutput]}]);
run_computers([{Pos, Mem, IP, waiting_for_input, _}|Rest]) ->
    {_, _, _, _, [AmpOutput]} = lists:last(Rest),
    {NewMem, NewIP, NewState, NewOutput} = exec(Mem, IP, [AmpOutput], []),
    run_computers(Rest ++ [{Pos, NewMem, NewIP, NewState, NewOutput}]).


peek(Mem, Address) ->
    lists:nth(Address + 1, Mem).


read(Mem, Address, Mode) ->
    Index = peek(Mem, Address),
    case Mode of
        indirect -> peek(Mem, Index);
        direct -> Index
    end.


poke(Mem, Address, Value) ->
    lists:sublist(Mem, Address) ++ [Value] ++ lists:nthtail(Address + 1, Mem).


param_mode(ParamSpec) ->
    case ParamSpec of
        0 -> indirect;
        1 -> direct
    end.


param_1_mode(C) ->
    param_mode((C div 100) rem 10).


param_2_mode(C) ->
    param_mode((C div 1000) rem 10).


exec(Mem, IP, Input, Output) ->
    C = peek(Mem, IP),
    case C rem 100 of
        1 -> % add
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            NewMem = poke(Mem, ResultAddress, A + B),
            exec(NewMem, IP + 4, Input, Output);
        2 -> % mul
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            NewMem = poke(Mem, ResultAddress, A * B),
            exec(NewMem, IP + 4, Input, Output);
        3 -> % input
            ResultAddress = peek(Mem, IP + 1),
            case Input of
                [InputValue|Rest] ->
                    NewMem = poke(Mem, ResultAddress, InputValue),
                    exec(NewMem, IP + 2, Rest, Output);
                [] ->
                    {Mem, IP, waiting_for_input, Output}
            end;
        4 -> % output
            A = read(Mem, IP + 1, param_1_mode(C)),
            exec(Mem, IP + 2, Input, Output ++ [A]);
        5 -> % jump if true
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            case A of
                0 -> exec(Mem, IP + 3, Input, Output);
                _ -> exec(Mem, B, Input, Output)
            end;
        6 -> % jump if false
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            case A of
                0 -> exec(Mem, B, Input, Output);
                _ -> exec(Mem, IP + 3, Input, Output)
            end;
        7 -> % less than
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            R = if A < B -> 1; true -> 0 end,
            NewMem = poke(Mem, ResultAddress, R),
            exec(NewMem, IP + 4, Input, Output);
        8 -> % equals
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            R = if A =:= B -> 1; true -> 0 end,
            NewMem = poke(Mem, ResultAddress, R),
            exec(NewMem, IP + 4, Input, Output);
        99 -> % halt
            {Mem, IP, halted, Output}
    end.


parse_line(CurrentInstruction, [$,|Rest]) ->
    [list_to_integer(CurrentInstruction)] ++ parse_line("", Rest);
parse_line(CurrentInstruction, "\n") ->
    [list_to_integer(CurrentInstruction)];
parse_line(CurrentInstruction, "") ->
    [list_to_integer(CurrentInstruction)];
parse_line(CurrentInstruction, [A|Rest]) ->
    parse_line(CurrentInstruction ++ [A], Rest).


load_program(Device) ->
    case io:get_line(Device, "") of
        eof  -> 
            [];
        Line -> 
            parse_line("", Line)
    end.
