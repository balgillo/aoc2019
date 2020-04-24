-module(day9_boost_part_1).

-export([run_boost_program/1]).

-record(computer, {
    mem, % memory contents
    ip, % instruction pointer
    relative_base, % base for relative mode access
    state, % waiting for input or halted?
    inputs, % list of available input values
    outputs, % list of available output values
    output_connections
}).



run_boost_program(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    InitMem = try load_program(Device)
      after file:close(Device)
    end,
    Computer = exec(#computer{
        mem=InitMem, ip=0, relative_base=0, state=ready, 
        inputs=[1], 
        outputs=[], output_connections=output
    }),
    Computer#computer.outputs.


peek(Mem, Address) when Address < length(Mem) ->
    lists:nth(Address + 1, Mem);
peek(_, _) ->
    0.


read(Mem, Address, RelativeBase, Mode) ->
    Index = peek(Mem, Address),
    case Mode of
        indirect -> peek(Mem, Index);
        direct -> Index;
        relative -> peek(Mem, RelativeBase + Index)
    end.


address_from(Mem, Address, RelativeBase, Mode) ->
    Index = peek(Mem, Address),
    case Mode of
        indirect -> Index;
        direct -> error;
        relative -> RelativeBase + Index
    end.


poke(Mem, Address, Value) when Address < length(Mem) ->
    lists:sublist(Mem, Address) ++ [Value] ++ lists:nthtail(Address + 1, Mem);
poke(Mem, Address, Value) when Address =:= length(Mem) ->
    Mem ++ [Value];
poke(Mem, Address, Value) ->
    poke(Mem ++ [0], Address, Value).


param_mode(ParamSpec) ->
    case ParamSpec of
        0 -> indirect;
        1 -> direct;
        2 -> relative
    end.


read_param_1(#computer{mem=Mem, ip=IP, relative_base=RelativeBase}) ->
    read(Mem, IP + 1, RelativeBase, param_mode((peek(Mem, IP) div 100) rem 10)).


read_param_2(#computer{mem=Mem, ip=IP, relative_base=RelativeBase}) ->
    read(Mem, IP + 2, RelativeBase, param_mode((peek(Mem, IP) div 1000) rem 10)).


address_from_param_1(#computer{mem=Mem, ip=IP, relative_base=RelativeBase}) ->
    address_from(Mem, IP + 1, RelativeBase, param_mode((peek(Mem, IP) div 100) rem 10)).


address_from_param_3(#computer{mem=Mem, ip=IP, relative_base=RelativeBase}) ->
    address_from(Mem, IP + 3, RelativeBase, param_mode((peek(Mem, IP) div 10000) rem 10)).


exec(Computer) ->
    Mem = Computer#computer.mem,
    IP = Computer#computer.ip,
    Cmd = peek(Mem, IP),
    case Cmd rem 100 of
        1 -> % add
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            ResultAddress = address_from_param_3(Computer),
            exec(Computer#computer{mem=poke(Mem, ResultAddress, A + B), ip=IP + 4});
        2 -> % mul
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            ResultAddress = address_from_param_3(Computer),
            exec(Computer#computer{mem=poke(Mem, ResultAddress, A * B), ip=IP + 4});
        3 -> % input
            ResultAddress = address_from_param_1(Computer),
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
            ResultAddress = address_from_param_3(Computer),
            R = if A < B -> 1; true -> 0 end,
            exec(Computer#computer{mem=poke(Mem, ResultAddress, R), ip=IP + 4});
        8 -> % equals
            A = read_param_1(Computer),
            B = read_param_2(Computer),
            ResultAddress = address_from_param_3(Computer),
            R = if A =:= B -> 1; true -> 0 end,
            exec(Computer#computer{mem=poke(Mem, ResultAddress, R), ip=IP + 4});
        9 -> % add to relative base
            A = read_param_1(Computer),
            exec(Computer#computer{relative_base=Computer#computer.relative_base + A, ip=IP + 2});
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
