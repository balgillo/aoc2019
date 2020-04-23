To compile:

```
rustc day5_test.rs
````

To run:

```
./day5_test ../data/3_0_4_0_99.csv
# Input printed as output

./day5_test ../data/1002_4_3_4_33.csv
# [1002,4,3,4,99]

./day5_test ../data/day5_part_1.csv

Input:1
Output: 0
Output: 0
Output: 0
Output: 0
Output: 0
Output: 0
Output: 0
Output: 0
Output: 0
Output: 7265618
[3,225,1,225,6,6,1101,1,238,225,104,0,1102,59,58,224,1001,
 224,-3422,224,4,224,102,8,223,223,101,3,224|...]


rustc day5_test_part_2.rs

./day5_test_part_2 ../data/9_9_8_9.csv
# - Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
./day5_test_part_2 ../data/3_9_7_9.csv
# - Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
./day5_test_part_2 ../data/3_3_1108.csv
# - Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
./day5_test_part_2 ../data/3_3_1107.csv
# - Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).

./day5_test_part_2 ../data/3_12_6_12.csv
#  output 0 if the input was zero or 1 if the input was non-zero
./day5_test_part_2 ../data/3_3_1105.csv
#  output 0 if the input was zero or 1 if the input was non-zero

./day5_test_part_2 ../data/3_21_1008.csv
# ask for a single number. The program will then output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8.

./day5_test_part_2 ../data/day5_part_2.csv
Input: 5
Output: 7731427
[314,225,1,225,6,6,1105,1,238,225,104,0,1102,59,58,224,1001,
 224,-3422,224,4,224,102,8,223,223,101,3,224|...]

 ```