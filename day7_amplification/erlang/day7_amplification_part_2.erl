-module(day7_amplification_part_2).

-export([max_amp_series_output/1]).

-record(computer, {
    mem, % memory contents
    ip, % instruction pointer
    state, % waiting for input or halted?
    inputs, % list of available input values
    outputs, % list of available output values
    output_connections % circuit or circuit_and_output
}).

% lehmer_code is a factoradic number ([0] = 1s digit, [1] = 2s digit etc.) that can be turned into
% a permutation of self.items via the "Lehmer code"
% See https://en.wikipedia.org/wiki/Factorial_number_system#Permutations
-record(permutation_generator, {
    lehmer_code, % The factoradic number representing the Lehmer code
    items % The items to be permuted
}).


max_amp_series_output(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    InitMem = try load_program(Device)
      after file:close(Device)
    end,
    calc_max_amp_series_output(InitMem, [5, 6, 7, 8, 9]).

calc_max_amp_series_output(InitMem, AmpPhaseValues) ->
    Permutation = #permutation_generator{lehmer_code=[0 || _ <- AmpPhaseValues], items=AmpPhaseValues},
    calc_max_amp_series_output_for_permutation(InitMem, Permutation, 0).


calc_max_amp_series_output_for_permutation(InitMem, AmpPhasesPermutationGenerator, HighestOutput) ->
    AmpPhases = generate_permutation(AmpPhasesPermutationGenerator),
    AmpOutput = calc_amp_series_output(InitMem, AmpPhases),
    NewHighestOutput = 
        if 
            AmpOutput > HighestOutput -> 
                io:fwrite("~w improved output to ~w~n", [AmpPhases, AmpOutput]),
                AmpOutput;
            true -> HighestOutput
        end,
    NextPermutation = next_permutation(AmpPhasesPermutationGenerator),
    case NextPermutation of
        none -> NewHighestOutput;
        _ -> calc_max_amp_series_output_for_permutation(InitMem, NextPermutation, NewHighestOutput)
    end.


calc_amp_series_output(InitMem, AmpPhases) ->
    Computers = init_computers(InitMem, AmpPhases, 0),
    run_computers(Computers).


next_permutation(PermutationGenerator) ->
    NextLehmerCode = increment_factoradic(PermutationGenerator#permutation_generator.lehmer_code, 0),
    case NextLehmerCode of
        overflow -> none;
        _ -> PermutationGenerator#permutation_generator{lehmer_code=NextLehmerCode}
    end.


increment_factoradic([A|Rest], ValuePosition) when A < ValuePosition ->
    [A + 1|Rest];
increment_factoradic([_|Rest], ValuePosition) ->
    Next = increment_factoradic(Rest, ValuePosition + 1),
    case Next of
        overflow -> overflow;
        _ -> [0|increment_factoradic(Rest, ValuePosition + 1)]
    end;
increment_factoradic([], _) ->
    overflow.


generate_permutation(PermutationGenerator) ->
    build_permutation(PermutationGenerator#permutation_generator.items, 
                    lists:reverse(PermutationGenerator#permutation_generator.lehmer_code)).


build_permutation(Items, [Position|Rest]) ->
    Item = lists:nth(Position + 1, Items),
    [Item|build_permutation(lists:sublist(Items, Position) ++ lists:nthtail(Position + 1, Items), Rest)];
build_permutation([], []) ->
    [].


init_computers(InitMem, [AmpPhase|Rest], PreviousAmpOutput) ->
    OutputConnections = case Rest of
        [] -> circuit_and_output;
        _ -> circuit
    end,
    Computer = exec(#computer{
        mem=InitMem, ip=0, state=ready, 
        inputs=[AmpPhase, PreviousAmpOutput], 
        outputs=[], output_connections=OutputConnections
    }),
    [AmpOutput] = Computer#computer.outputs,
    [Computer] ++ init_computers(InitMem, Rest, AmpOutput);
init_computers(_, [], _) ->
    [].


% circular buffer of computers.  The first item in the buffer is the next computer to run,
% the last item is the previously-run computer.
run_computers([#computer{state=halted, output_connections=circuit_and_output, outputs=[AmpOutput]}|_]) ->
    AmpOutput;
run_computers([Computer|Rest]) ->
    case Computer#computer.state of
        halted -> 
            run_computers(Rest ++ [Computer]);
        waiting_for_input ->
            #computer{outputs=[AmpOutput]} = lists:last(Rest),
            UpdatedComputer = exec(Computer#computer{inputs=[AmpOutput], outputs=[]}),
            run_computers(Rest ++ [UpdatedComputer])
    end.


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


read_param_1(Computer) ->
    Mem = Computer#computer.mem,
    IP = Computer#computer.ip,
    read(Mem, IP + 1, param_mode((peek(Mem, IP) div 100) rem 10)).


read_param_2(Computer) ->
    Mem = Computer#computer.mem,
    IP = Computer#computer.ip,
    read(Mem, IP + 2, param_mode((peek(Mem, IP) div 1000) rem 10)).


exec(Computer) ->
    Mem = Computer#computer.mem,
    IP = Computer#computer.ip,
    Cmd = peek(Mem, IP),
    case Cmd rem 100 of
        1 -> % add
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            ResultAddress = peek(Mem, IP + 3),
            exec(Computer#computer{mem=poke(Mem, ResultAddress, A + B), ip=IP + 4});
        2 -> % mul
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            ResultAddress = peek(Mem, IP + 3),
            exec(Computer#computer{mem=poke(Mem, ResultAddress, A * B), ip=IP + 4});
        3 -> % input
            ResultAddress = peek(Mem, IP + 1),
            case Computer#computer.inputs of
                [InputValue|Rest] ->
                    exec(Computer#computer{mem=poke(Mem, ResultAddress, InputValue), ip=IP + 2, inputs=Rest});
                [] ->
                    Computer#computer{state=waiting_for_input}
            end;
        4 -> % output
            A = read_param_1(Computer),
            exec(Computer#computer{ip=IP + 2, outputs=Computer#computer.outputs ++ [A]});
        5 -> % jump if true
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            case A of
                0 -> 
                    exec(Computer#computer{ip=IP + 3});
                _ ->
                    exec(Computer#computer{ip=B})
            end;
        6 -> % jump if false
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            case A of
                0 ->
                    exec(Computer#computer{ip=B});
                _ -> 
                    exec(Computer#computer{ip=IP + 3})
            end;
        7 -> % less than
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            ResultAddress = peek(Mem, IP + 3),
            R = if A < B -> 1; true -> 0 end,
            exec(Computer#computer{mem=poke(Mem, ResultAddress, R), ip=IP + 4});
        8 -> % equals
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            ResultAddress = peek(Mem, IP + 3),
            R = if A =:= B -> 1; true -> 0 end,
            exec(Computer#computer{mem=poke(Mem, ResultAddress, R), ip=IP + 4});
        99 -> % halt
            Computer#computer{state=halted}
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
