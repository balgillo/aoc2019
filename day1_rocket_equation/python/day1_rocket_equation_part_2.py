#!/bin/python

import sys
import io

def module_mass(m):
    f = (m // 3) - 2
    if f <= 0:
        return 0
    else:
        return f + module_mass(f)


file_path = sys.argv[1]
masses = []
with io.open(file_path, "r") as f:
    while True:
        line = f.readline()
        if line:
            masses.append(int(line.strip()))
        else:
            break

total_fuel = sum([module_mass(m) for m in masses])

print("{}".format(total_fuel))
