#!/bin/python

import sys
import io

def count_orbits(satellite, graph):
    p = satellite
    total = 0
    while True:
        p = graph.get(p)
        if p == None:
            return total
        else:
            total += 1


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

total_orbits = 0
for satellite in graph:
    total_orbits += count_orbits(satellite, graph)


print(total_orbits)
