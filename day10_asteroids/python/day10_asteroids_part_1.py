#!/bin/python

import sys
import io
import math
from enum import Enum

class AsteroidMap:
    def __init__(self):
        self.asteroid_present = []
        self.asteroid_count = 0
        self.map_width = None
        self.map_height = None
        self.visible_asteroids = None

    def load(self, f):
        while True:
            line = f.readline()
            if line:
                line = line.strip()
                if self.map_width:
                    if len(line) != self.map_width:
                        raise Exception("Jagged input")
                else:
                    self.map_width = len(line)
                asteroid_row = []
                for x in line:
                    if x == '#':
                        asteroid_row.append(True)
                        self.asteroid_count += 1
                    else:
                        asteroid_row.append(False)
                self.asteroid_present.append(asteroid_row)
            else:
                break
        self.map_height = len(self.asteroid_present)

    def is_asteroid_present(self, x, y):
        return self.asteroid_present[y][x]

    def calc_visibility_from_all_asteroids(self):
        # Start by assuming every asteroid can see every other asteroid
        self.visible_asteroids = [[self.asteroid_count - 1 if c else 0 for c in row] for row in self.asteroid_present]

        # Now occlude lines of sight
        # verticals
        self.discount_occluded_in_lines(0, 1)
        # horizontals
        self.discount_occluded_in_lines(1, 0)
        # diagonals
        for x_step in range(1, self.map_width):
            for y_step in range(1, self.map_height):
                # x and y steps must be coprime to avoid double counting
                if math.gcd(x_step, y_step) == 1:
                    self.discount_occluded_in_lines(-x_step, y_step)
                    self.discount_occluded_in_lines(x_step, y_step)

    def discount_occluded_in_lines(self, x_step, y_step):
        # Starting positions to cover the rectangle with lines stepping (x_step, y_step):
        # Top rows 0, 1, ..., y_step - 1
        # If x_step > 0 then left columns 0, 1, ..., x_step - 1
        # If x_step < 0 then right columns map_width + x_step, map_width + x_step + 1, ..., map_width - 1 
        # (skip top y_step rows of the columns to avoid double-counting in the corner)
        for y_start in range(y_step):
            for x_start in range (self.map_width):
                self.discount_occluded_in_line(x_start, y_start, x_step, y_step)
        
        (x_start_min_incl, x_start_max_excl) = (0, x_step) if x_step > 0 else (self.map_width + x_step, self.map_width)

        for y_start in range(y_step, self.map_height):
            for x_start in range(x_start_min_incl, x_start_max_excl):
                self.discount_occluded_in_line(x_start, y_start, x_step, y_step)

    def discount_occluded_in_line(self, x_start, y_start, x_step, y_step):
        # Trace the line (x_start, y_start), (x_start + x_step, y_start + y_step), (x_start + x_step * 2, y_start + y_step * 2) etc.
        asteroid_count_in_line = 0
        (x, y) = (x_start, y_start)
        while x >= 0 and x < self.map_width and y >= 0 and y < self.map_height:
            if self.is_asteroid_present(x, y):
                asteroid_count_in_line += 1
            (x, y) = (x + x_step, y + y_step)
        if asteroid_count_in_line >= 3:
            # Some asteroids are occluded in this line.
            # The first and last asteroid in the line are shielded from seeing (asteroid_count_in_line - 2) other asteroids in the line.
            # The other asteroids in the line are shielded from seeing (asteroid_count_in_line - 3) other asteroids in the line.
            asteroid_index_in_line = 0
            # 3 in line #.#.# - 1 hidden from the end asteroids, 0 hidden from the middle asteroids
            # 4 in line #.#.#.# - 2 hidden from the end asteroids, 1 hidden from the middle asteroids
            (x, y) = (x_start, y_start)
            while x >= 0 and x < self.map_width and y >= 0 and y < self.map_height:
                if self.is_asteroid_present(x, y):
                    at_end_of_line = asteroid_index_in_line in [0, asteroid_count_in_line - 1]
                    self.visible_asteroids[y][x] -= (asteroid_count_in_line - 2) if at_end_of_line else (asteroid_count_in_line - 3)
                    asteroid_index_in_line += 1
                (x, y) = (x + x_step, y + y_step)        
    
    def get_best_visibility_asteroid(self):
        (best_pos_x, best_pos_y, best_pos_visible_asteroids) = (-1, -1, 0)

        for y in range(0, self.map_height):
            row = self.visible_asteroids[y]
            for x in range(0, self.map_width):
                visible_count = row[x]
                if visible_count > best_pos_visible_asteroids:
                    (best_pos_x, best_pos_y, best_pos_visible_asteroids) = (x, y, visible_count)
        return (best_pos_x, best_pos_y, best_pos_visible_asteroids)

    def calc_visibility_from_asteroid(self, x_pos, y_pos):
        visible = 0
        dirs = set()
        for y in range(0, self.map_height):
            for x in range(0, self.map_width):
                if self.is_asteroid_present(x, y) and (x, y) != (x_pos, y_pos):
                    # is this asteroid visible from best_pos_x, best_pos_y?
                    (xo, yo) = (x - x_pos, y - y_pos)
                    g = math.gcd(xo, yo)
                    # d represents a direction via its slope.  It uniquely corresponds to a value in the
                    # affinely-extended rational number line (Q u {+infinity, -infinity} where infinity = up
                    # and -infinity = down).
                    d = (xo / g, yo / g)
                    if d not in dirs:
                        dirs.add(d)
                        visible += 1
        return visible


asteroid_map = AsteroidMap()
file_path = sys.argv[1]
with io.open(file_path, "r") as f:
    asteroid_map.load(f)

asteroid_map.calc_visibility_from_all_asteroids()

(best_pos_x, best_pos_y, best_pos_visible_asteroids) = asteroid_map.get_best_visibility_asteroid()


# Double-check result.  Can the best position really see that many other asteroids?

visible_from_best = asteroid_map.calc_visibility_from_asteroid(best_pos_x, best_pos_y)

if visible_from_best != best_pos_visible_asteroids:
    raise Exception("Counting error: thought {} visible from ({}, {}) but {} visible".format(best_pos_visible_asteroids, best_pos_x, best_pos_y, visible_from_best))

# Double-check result.  is it best?

for x in range(0, asteroid_map.map_width):
    for y in range(0, asteroid_map.map_height):
        if asteroid_map.is_asteroid_present(x, y):
            visible = asteroid_map.calc_visibility_from_asteroid(x, y)
            if visible > visible_from_best:
                print("Didn't find this better one!  ({},{}) can see {} asteroids".format(x, y, visible))

print("Best monitoring station is at ({}, {}), from where it can detect {} asteroids".format(best_pos_x, best_pos_y, best_pos_visible_asteroids))
