#!/bin/python

import sys
import io
from computer import IntCodeComputer

class Robot:
    def __init__(self, init_mem):
        self.computer = IntCodeComputer(init_mem, [])
        self.pos = (0, 0)
        self.direction = (0, -1)
        self.painted_panels = {}
        # initial panel is white
        self.paint_panel(1)
    
    def get_panel_colour(self, position):
        return self.painted_panels.get(position, 0) # painted colour or black if never painted
    
    def paint_panel(self, colour):
        self.painted_panels[self.pos] = colour

    def run(self):
        self.computer.inputs = [self.get_panel_colour(self.pos)]
        self.computer.run()
        if self.computer.is_halted():
            return False
        else:
            [new_colour, new_direction] = self.computer.outputs
            self.paint_panel(new_colour)
            (xd, yd) = self.direction
            if new_direction == 0:
                # turn left
                self.direction = (yd, -xd)
            elif new_direction == 1:
                # turn right
                self.direction = (-yd, xd)
            else:
                raise Exception("Unrecognised direction code")
            (x, y) = self.pos
            (xd, yd) = self.direction
            self.pos = (x + xd, y + yd)
            return True

    def print_panel_paint_pattern(self):
        (x_min, y_min, x_max, y_max) = (0, 0, 0, 0)
        for (x, y) in self.painted_panels:
            if x < x_min:
                x_min = x
            elif x > x_max:
                x_max = x
            if y < y_min:
                y_min = y
            elif y > y_max:
                y_max = y
        for y in range(y_min, y_max + 1):
            l = ""
            for x in range(x_min, x_max + 1):
                l += "XX" if self.get_panel_colour((x, y)) else "  "
            print(l)


file_path = "../data/puzzle_input.csv"
init_mem = []
with io.open(file_path, "r") as f:
    line = f.readline()
    if line:
        init_mem = [int(x.strip()) for x in line.split(",")]
    else:
        raise Exception("No program in file!")

robot = Robot(init_mem)
while robot.run():
    pass

robot.print_panel_paint_pattern()
