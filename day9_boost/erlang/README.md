To run:

```
erl
```

then in the Erlang shell:

```
c("day9_boost_part_1.erl", [debug_info]).

day9_boost_part_1:run_boost_program("../data/109,1,204.csv").
# [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

day9_boost_part_1:run_boost_program("../data/1102,34915192.csv").
# [1219070632396864]

day9_boost_part_1:run_boost_program("../data/104,1125899906842624.csv").
# [1125899906842624]

day9_boost_part_1:run_boost_program("../data/puzzle_input.csv").
# [2682107844]


c("day9_boost_part_2.erl", [debug_info]).

day9_boost_part_2:run_boost_program("../data/puzzle_input.csv").
# [34738]

```