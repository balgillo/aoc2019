#!/bin/python

import sys
import io

file_path = sys.argv[1]
masses = []
with io.open(file_path, "r") as f:
    while True:
        line = f.readline()
        if line:
            masses.append(int(line.strip()))
        else:
            break

total_fuel = sum([(m // 3) - 2 for m in masses])

print("{}".format(total_fuel))
