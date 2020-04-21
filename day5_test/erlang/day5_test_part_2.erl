-module(day5_test_part_2).

-export([run/1]).


run(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    Mem = try load_program(Device)
      after file:close(Device)
    end,
    exec(Mem, 0).


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


exec(Mem, I) ->
    C = lists:nth(I + 1, Mem),
    case C rem 100 of
        1 -> % add
            A = read(Mem, I + 1, param_1_mode(C)),
            B = read(Mem, I + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, I + 3),
            NewMem = poke(Mem, ResultAddress, A + B),
            exec(NewMem, I + 4);
        2 -> % mul
            A = read(Mem, I + 1, param_1_mode(C)),
            B = read(Mem, I + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, I + 3),
            NewMem = poke(Mem, ResultAddress, A * B),
            exec(NewMem, I + 4);
        3 -> % input
            ResultAddress = peek(Mem, I + 1),
            case io:fread("Input: ", "~d") of
                {ok, [InputValue]} ->
                    NewMem = poke(Mem, ResultAddress, InputValue),
                    exec(NewMem, I + 2);
                _ -> fail
            end;
        4 -> % output
            A = read(Mem, I + 1, param_1_mode(C)),
            io:fwrite("~s~w~n", ["Output: ", A]),
            exec(Mem, I + 2);
        5 -> % jump if true
            A = read(Mem, I + 1, param_1_mode(C)),
            B = read(Mem, I + 2, param_2_mode(C)),
            case A of
                0 -> exec(Mem, I + 3);
                _ -> exec(Mem, B)
            end;
        6 -> % jump if false
            A = read(Mem, I + 1, param_1_mode(C)),
            B = read(Mem, I + 2, param_2_mode(C)),
            case A of
                0 -> exec(Mem, B);
                _ -> exec(Mem, I + 3)
            end;
        7 -> % less than
            A = read(Mem, I + 1, param_1_mode(C)),
            B = read(Mem, I + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, I + 3),
            R = if A < B -> 1; true -> 0 end,
            NewMem = poke(Mem, ResultAddress, R),
            exec(NewMem, I + 4);
        8 -> % equals
            A = read(Mem, I + 1, param_1_mode(C)),
            B = read(Mem, I + 2, param_2_mode(C)),
            ResultAddress = peek(Mem, I + 3),
            R = if A =:= B -> 1; true -> 0 end,
            NewMem = poke(Mem, ResultAddress, R),
            exec(NewMem, I + 4);
        99 -> % halt
            Mem
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
