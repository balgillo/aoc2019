-module(day7_amplification_part_1).

-include_lib("eunit/include/eunit.hrl").

-export([max_amp_series_output/1]).


max_amp_series_output(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    InitMem = try load_program(Device)
      after file:close(Device)
    end,
    calc_max_amp_series_output(InitMem, [4, 3, 2, 1, 0], 0).


calc_max_amp_series_output(InitMem, AmpPhases, HighestOutput) ->
    AmpOutput = calc_amp_series_output(InitMem, AmpPhases, 0),
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


calc_amp_series_output(InitMem, [ThisAmpPhase|Rest], PreviousAmpOutput) ->
    InputFunction = 
        fun(InputIndex) -> 
            case InputIndex of
                0 -> ThisAmpPhase;
                1 -> PreviousAmpOutput
            end
        end,
    case exec(InitMem, 0, InputFunction, 0, []) of
        [OutputValue] -> calc_amp_series_output(InitMem, Rest, OutputValue);
        _ -> fail
    end;
calc_amp_series_output(_, [], PreviousAmpOutput) ->
    PreviousAmpOutput.


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


exec(Mem, IP, InputFun, InputCount, Output) ->
    C = peek(Mem, IP),
    case C rem 100 of
        1 -> % add
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            NewMem = poke(Mem, ResultAddress, A + B),
            exec(NewMem, IP + 4, InputFun, InputCount, Output);
        2 -> % mul
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            NewMem = poke(Mem, ResultAddress, A * B),
            exec(NewMem, IP + 4, InputFun, InputCount, Output);
        3 -> % input
            ResultAddress = peek(Mem, IP + 1),
            InputValue = InputFun(InputCount),
            NewMem = poke(Mem, ResultAddress, InputValue),
            exec(NewMem, IP + 2, InputFun, InputCount + 1, Output);
        4 -> % output
            A = read(Mem, IP + 1, param_1_mode(C)),
            exec(Mem, IP + 2, InputFun, InputCount, Output ++ [A]);
        5 -> % jump if true
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            case A of
                0 -> exec(Mem, IP + 3, InputFun, InputCount, Output);
                _ -> exec(Mem, B, InputFun, InputCount, Output)
            end;
        6 -> % jump if false
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            case A of
                0 -> exec(Mem, B, InputFun, InputCount, Output);
                _ -> exec(Mem, IP + 3, InputFun, InputCount, Output)
            end;
        7 -> % less than
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            R = if A < B -> 1; true -> 0 end,
            NewMem = poke(Mem, ResultAddress, R),
            exec(NewMem, IP + 4, InputFun, InputCount, Output);
        8 -> % equals
            A = read(Mem, IP + 1, param_1_mode(C)),
            B = read(Mem, IP + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, IP + 3),
            R = if A =:= B -> 1; true -> 0 end,
            NewMem = poke(Mem, ResultAddress, R),
            exec(NewMem, IP + 4, InputFun, InputCount, Output);
        99 -> % halt
            Output
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
