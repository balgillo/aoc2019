#!/bin/python

import sys
import io
from enum import Enum

class Direction(Enum):
    up = 1
    down = 2
    left = 3
    right = 4


class Move:
    def __init__(self, direction, distance):
        self.direction = direction
        self.distance = distance

    def __repr__(self):
        return "{} {}".format(self.direction, self.distance)

    def apply(self, x0, y0):
        if self.direction == Direction.up:
            return (x0, y0 - self.distance)
        elif self.direction == Direction.down:
            return (x0, y0 + self.distance)
        elif self.direction == Direction.left:
            return (x0 - self.distance, y0)
        elif self.direction == Direction.right:
            return (x0 + self.distance, y0)

    def to_segment(self, x0, y0):
        if self.direction == Direction.up:
            return WireSegment(x0, y0 - self.distance, x0, y0)
        elif self.direction == Direction.down:
            return WireSegment(x0, y0, x0, y0 + self.distance)
        elif self.direction == Direction.left:
            return WireSegment(x0 - self.distance, y0, x0, y0)
        elif self.direction == Direction.right:
            return WireSegment(x0, y0, x0 + self.distance, y0)


class WireSegment:
    def __init__(self, x0, y0, x1, y1):
        self.x0 = x0
        self.y0 = y0
        self.x1 = x1
        self.y1 = y1

    def is_horizontal(self):
        return self.y0 == self.y1

    def is_vertical(self):
        return self.x0 == self.x1

    def crossing_point(self, other_wire_segment):
        if self.is_horizontal():
            if other_wire_segment.is_horizontal():
                return None
            elif other_wire_segment.x0 >= self.x0 and other_wire_segment.x0 <= self.x1 and self.y0 >= other_wire_segment.y0 and self.y0 <= other_wire_segment.y1:
                return (other_wire_segment.x0, self.y0)
            else:
                return None
        else: # this segment is vertical
            if other_wire_segment.is_vertical():
                return None
            elif other_wire_segment.y0 >= self.y0 and other_wire_segment.y0 <= self.y1 and self.x0 >= other_wire_segment.x0 and self.x0 <= other_wire_segment.x1:
                return (self.x0, other_wire_segment.y0)
            else:
                return None


def manhattan_distance_from_origin(x, y):
    return abs(x) + abs(y)


def parse_direction(c):
    if c == 'U':
        return Direction.up
    elif c == 'D':
        return Direction.down
    elif c == 'L':
        return Direction.left
    elif c == 'R':
        return Direction.right


def parse_move(m):
    return Move(parse_direction(m[0]), int(m[1:]))


def closest(d1, d2):
    if not d1:
        return d2
    elif not d2:
        return d1
    elif d1 < d2:
        return d1
    else:
        return d2


def find_closest_crossing(wire1_moves, wire2_moves):
    wire1_x, wire1_y = 0, 0
    best_crossing_distance = None
    for wire1_move in wire1_moves:
        wire1_segment = wire1_move.to_segment(wire1_x, wire1_y)
        wire2_x, wire2_y = 0, 0
        for wire2_move in wire2_moves:
            wire2_segment = wire2_move.to_segment(wire2_x, wire2_y)
            c = wire1_segment.crossing_point(wire2_segment)
            if c:
                (cx, cy) = c
                best_crossing_distance = closest(best_crossing_distance, manhattan_distance_from_origin(cx, cy))
            (wire2_x, wire2_y) = wire2_move.apply(wire2_x, wire2_y)
        (wire1_x, wire1_y) = wire1_move.apply(wire1_x, wire1_y)
    return best_crossing_distance


file_path = sys.argv[1]
wire1_moves = []
wire2_moves = []
with io.open(file_path, "r") as f:
    wire1_moves = [parse_move(m) for m in f.readline().split(",")]
    wire2_moves = [parse_move(m) for m in f.readline().split(",")]

closest_crossing_distance = find_closest_crossing(wire1_moves, wire2_moves)

print(closest_crossing_distance)
