# Note: AMS uses OTP 19, my dev environment is on OTP 16
 
erl

c("day3_crossed_wires.erl").

day3_crossed_wires:find_closest_crossing("../data/example1.csv").
# 6

day3_crossed_wires:find_closest_crossing("../data/example2.csv").
# 159

day3_crossed_wires:find_closest_crossing("../data/example3.csv").
# 135

day3_crossed_wires:find_closest_crossing("../data/wires.csv").
# 2129

c("day3_crossed_wires_part_2.erl").

day3_crossed_wires_part_2:find_closest_crossing("../data/example1.csv").
# 30

day3_crossed_wires_part_2:find_closest_crossing("../data/example2.csv").
# 610

day3_crossed_wires_part_2:find_closest_crossing("../data/example3.csv").
# 410

day3_crossed_wires_part_2:find_closest_crossing("../data/wires.csv").
# 134662
