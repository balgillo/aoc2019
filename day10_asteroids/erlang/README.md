```
erl
```

Then in erlang shell:

```

c("day10_asteroids_part_1").

day10_asteroids_part_1:find_best_monitoring_point("../data/small_example.txt").
{{point,3,4},8}

day10_asteroids_part_1:find_best_monitoring_point("../data/medium_example_1.txt").
{{point,5,8},33}

day10_asteroids_part_1:find_best_monitoring_point("../data/medium_example_2.txt").
{{point,1,2},35}

day10_asteroids_part_1:find_best_monitoring_point("../data/medium_example_3.txt").
{{point,6,3},41}

day10_asteroids_part_1:find_best_monitoring_point("../data/large_example.txt").
{{point,11,13},210}

day10_asteroids_part_1:find_best_monitoring_point("../data/puzzle_input.txt").
{{point,30,34},344}


c("day10_asteroids_part_2").
day10_asteroids_part_2:blast_asteroids("../data/blast_example.txt").
Blasting asteroids from {point,8,3} from where 30 asteroids are visible.
Blasted asteroid #1 at {point,8,1}
Blasted asteroid #2 at {point,9,0}
Blasted asteroid #3 at {point,9,1}
Blasted asteroid #4 at {point,10,0}
Blasted asteroid #5 at {point,9,2}
Blasted asteroid #6 at {point,11,1}
Blasted asteroid #7 at {point,12,1}
Blasted asteroid #8 at {point,11,2}
Blasted asteroid #9 at {point,15,1}
Blasted asteroid #10 at {point,12,2}
Blasted asteroid #11 at {point,13,2}
Blasted asteroid #12 at {point,14,2}
Blasted asteroid #13 at {point,15,2}
Blasted asteroid #14 at {point,12,3}
Blasted asteroid #15 at {point,16,4}
Blasted asteroid #16 at {point,15,4}
Blasted asteroid #17 at {point,10,4}
Blasted asteroid #18 at {point,4,4}
Blasted asteroid #19 at {point,2,4}
Blasted asteroid #20 at {point,2,3}
Blasted asteroid #21 at {point,0,2}
Blasted asteroid #22 at {point,1,2}
Blasted asteroid #23 at {point,0,1}
Blasted asteroid #24 at {point,1,1}
Blasted asteroid #25 at {point,5,2}
Blasted asteroid #26 at {point,1,0}
Blasted asteroid #27 at {point,5,1}
Blasted asteroid #28 at {point,6,1}
Blasted asteroid #29 at {point,6,0}
Blasted asteroid #30 at {point,7,0}
Blasted asteroid #31 at {point,14,3}
Blasted asteroid #32 at {point,16,1}
Blasted asteroid #33 at {point,14,0}
Blasted asteroid #34 at {point,10,1}
Blasted asteroid #35 at {point,8,0}
Blasted asteroid #36 at {point,13,3}
{}


day10_asteroids_part_2:blast_asteroids("../data/large_example.txt").
... Blasted asteroid #200 at {point,8,2} ...


day10_asteroids_part_2:blast_asteroids("../data/puzzle_input.txt").
... Blasted asteroid #200 at {point,27,32} ...

```
