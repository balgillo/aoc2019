#!/bin/python

import sys
import io
from computer import IntCodeComputer

class Robot:
    def __init__(self, init_mem):
        self.computer = IntCodeComputer(init_mem, [0])
        self.pos = (0, 0)
        self.direction = (0, -1)
        self.painted_panels = []
    
    def get_panel_colour(self):
        for (coords, colour) in self.painted_panels:
            if coords == self.pos:
                return colour
        return 0 # black
    
    def paint_panel(self, colour):
        self.painted_panels = \
            [(coords, colour) for (coords, colour) in self.painted_panels if coords != self.pos] + \
            [(self.pos, colour)]

    def run(self):
        self.computer.inputs = [self.get_panel_colour()]
        self.computer.run()
        if self.computer.is_halted():
            return False
        else:
            [new_colour, new_direction] = self.computer.outputs
            self.paint_panel(new_colour)
            (xd, yd) = self.direction
            if new_direction == 0:
                # turn left
                self.direction = (-yd, xd)
            elif new_direction == 1:
                # turn right
                self.direction = (yd, -xd)
            else:
                raise Exception("Unrecognised direction code")
            (x, y) = self.pos
            (xd, yd) = self.direction
            self.pos = (x + xd, y + yd)
            return True


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


print("Output: {}\n".format(len(robot.painted_panels)))
