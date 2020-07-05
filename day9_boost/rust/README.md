```
Some commands you might need to run to set things up
sudo yum install cargo

rustup install stable
rustup default stable

cargo build --manifest-path day9_boost_part_1/Cargo.toml
```

then to run:

```
./day9_boost_part_1/target/debug/day9_boost_part_1 ../data/109,1,204.csv
day9_boost_part_1\target\release\day9_boost_part_1.exe ../data/109,1,204.csv
# [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

./day9_boost_part_1/target/debug/day9_boost_part_1 ../data/1102,34915192.csv
day9_boost_part_1\target\release\day9_boost_part_1.exe ../data/1102,34915192.csv
# [1219070632396864]

./day9_boost_part_1/target/debug/day9_boost_part_1 ../data/104,1125899906842624.csv
day9_boost_part_1\target\release\day9_boost_part_1.exe ../data/104,1125899906842624.csv
# [1125899906842624]

./day9_boost_part_1/target/debug/day9_boost_part_1 ../data/puzzle_input.csv
day9_boost_part_1\target\release\day9_boost_part_1.exe ../data/puzzle_input.csv
# [2682107844]


cargo build --release --manifest-path day9_boost_part_2/Cargo.toml

./day9_boost_part_2/target/release/day9_boost_part_2 ../data/puzzle_input.csv
day9_boost_part_2\target\release\day9_boost_part_2.exe ../data/puzzle_input.csv
# [34738]

```