#!/bin/python

import sys
import io
import math
from enum import Enum

def quandrant(x0, y0):
    if x0 >= 0:
        return 0 if y0 <= 0 else 1
    else:
        return 2 if y0 > 0 else 3


def cmp_angle(d0, d1):
    """
    Compare two angles clockwise from up.  Return a number < 0 if d0 < d1, 0 if equal, > 0 if d0 > d1
    """
    (x0, y0) = d0
    (x1, y1) = d1
    q0 = quandrant(x0, y0)
    q1 = quandrant(x1, y1)
    if q0 != q1:
        return q0 - q1
    else:
        return x1 * y0 - y1 * x0


def cmp_length(d0, d1):
    (x0, y0) = d0
    (x1, y1) = d1
    return abs(x0) + abs(y0) - abs(x1) - abs(y1)


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

    def scan_for_other_asteroids(self, x_pos, y_pos):
        """
        An "asteroid radar" which looks for asteroids and sorts them by clock angle, then by distance.
        """
        ret = []
        for y0 in range(0, self.map_height):
            for x0 in range(0, self.map_width):
                if self.is_asteroid_present(x0, y0):
                    d0 = (x0 - x_pos, y0 - y_pos)
                    if d0 != (0, 0):
                        # insert into ret at the appropriate place
                        i0 = len(ret)
                        for i1 in range(len(ret)):
                            (x1, y1) =  ret[i1]
                            d1 = (x1 - x_pos, y1 - y_pos)
                            angle_cmp = cmp_angle(d0, d1)
                            if angle_cmp < 0:
                                # d0 earlier angle than d1, insert before
                                i0 = i1
                                break
                            if angle_cmp == 0:
                                # same angle, so compare length
                                length_cmp = cmp_length(d0, d1)
                                if length_cmp < 0:
                                    # same angle, this one's closer, so insert before
                                    i0 = i1
                                    break
                        ret.insert(i0, (x0, y0))
        return ret


asteroid_map = AsteroidMap()
file_path = sys.argv[1]
with io.open(file_path, "r") as f:
    asteroid_map.load(f)

asteroid_map.calc_visibility_from_all_asteroids()

(ms_x, ms_y, best_pos_visible_asteroids) = asteroid_map.get_best_visibility_asteroid()
ms = (ms_x, ms_y)

print("Best monitoring station is at ({}, {}), from where it can detect {} asteroids".format(ms_x, ms_y, best_pos_visible_asteroids))

other_asteroids_clockwise = asteroid_map.scan_for_other_asteroids(ms_x, ms_y)

# Now blast asteroids clockwise, but only the closest one in each line of sight each time
asteroids_to_blast = other_asteroids_clockwise
asteroids_to_blast_next_rotation = []
blast_number = 1
while asteroids_to_blast:
    # blast first asteroid
    a = asteroids_to_blast.pop(0)
    print("Blast #{}: {}".format(blast_number, a))
    blast_number += 1
    (a_x, a_y) = a
    last_blast_d = (a_x - ms_x, a_y - ms_y)
    # now do one rotation
    while asteroids_to_blast:
        a = asteroids_to_blast.pop(0)
        (a_x, a_y) = a
        d = (a_x - ms_x, a_y - ms_y)
        if cmp_angle(d, last_blast_d):
            print("Blast #{}: {}".format(blast_number, a))
            blast_number += 1
            last_blast_d = d
        else:
            asteroids_to_blast_next_rotation.append(a)
    asteroids_to_blast = asteroids_to_blast_next_rotation
    asteroids_to_blast_next_rotation = []


