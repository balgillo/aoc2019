#!/bin/python

import sys
import io

def orbit_path(satellite, graph):
    p = satellite
    ret = []
    while True:
        p = graph.get(p)
        if p == None:
            return ret
        else:
            ret.append(p)


file_path = sys.argv[1]
graph = {}
with io.open(file_path, "r") as f:
    while True:
        line = f.readline()
        if line:
            tokens = line.split(")")
            satellite = tokens[1].strip()
            centre = tokens[0].strip()
            graph[satellite] = centre
        else:
            break

your_path = orbit_path("YOU", graph)
santa_path = orbit_path("SAN", graph)

total_transfers = 0
common_orbit = None
for p in your_path:
    common_orbit = p
    if p in santa_path:
        break
    total_transfers += 1

for p in santa_path:
    if p == common_orbit:
        break
    total_transfers += 1

print(total_transfers)
