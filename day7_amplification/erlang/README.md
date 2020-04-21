To run:

```
erl
```

then in the Erlang shell:

```
c("day7_amplification_part_1.erl", [debug_info]).

day7_amplification_part_1:max_amp_series_output("../data/3,15,3.csv").
# 43210

day7_amplification_part_1:max_amp_series_output("../data/3,23,3.csv").
# 54321

day7_amplification_part_1:max_amp_series_output("../data/3,31,3.csv").
# 65210

day7_amplification_part_1:max_amp_series_output("../data/puzzle_input.csv").
# 46248


```