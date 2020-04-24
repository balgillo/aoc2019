#!/bin/python

import sys
import io
from enum import Enum



class ParamMode(Enum):
    indirect = 0
    direct = 1
    relative = 2

    @staticmethod
    def from_int(x):
        if x == 0:
            return ParamMode.indirect
        elif x == 1:
            return ParamMode.direct
        elif x == 2:
            return ParamMode.relative
        else:
            raise Exception("Bad param mode {}".format(x))


class ComputerState(Enum):
    ready = 0
    waiting_for_input = 1
    halted = 99


class IntCodeComputer:
    def __init__(self, init_mem, inputs):
        # copy memory using slice
        self.mem = init_mem[:]
        self.ip = 0
        self.relative_base = 0
        self.inputs = inputs
        self.outputs = []
        self.state = ComputerState.ready

    def peek(self, address):
        if address < len(self.mem):
            return self.mem[address]
        else:
            return 0

    def read(self, param_address, mode):
        if mode == ParamMode.indirect:
            return self.peek(self.peek(param_address))
        elif mode == ParamMode.direct:
            return self.peek(param_address)
        elif mode == ParamMode.relative:
            return self.peek(self.relative_base + self.peek(param_address))
        else:
            raise Exception("Bad read mode {}".format(mode))

    def read_param_1(self):
        return self.read(self.ip + 1, ParamMode.from_int((self.peek(self.ip) / 100) % 10))

    def read_param_2(self):
        return self.read(self.ip + 2, ParamMode.from_int((self.peek(self.ip) / 1000) % 10))

    def address_from_param(self, param_address, mode):
        if mode == ParamMode.indirect:
            return self.peek(param_address)
        elif mode == ParamMode.direct:
            raise Exception("Tried to take address of direct parameter")
        elif mode == ParamMode.relative:
            return self.relative_base + self.peek(param_address)
        else:
            raise Exception("Bad param mode {}".format(mode))

    def address_from_param_1(self):
        return self.address_from_param(self.ip + 1, ParamMode.from_int((self.peek(self.ip) / 100) % 10))

    def address_from_param_2(self):
        return self.address_from_param(self.ip + 2, ParamMode.from_int((self.peek(self.ip) / 1000) % 10))

    def address_from_param_3(self):
        return self.address_from_param(self.ip + 3, ParamMode.from_int((self.peek(self.ip) / 10000) % 10))

    def poke(self, address, value):
        if len(self.mem) <= address:
            while len(self.mem) < address:
                self.mem.append(0)
            self.mem.append(value)
        else:
            self.mem[address] = value

    def run(self):
        self.state = ComputerState.ready
        self.outputs = []
        while True:
            opcode = self.peek(self.ip) % 100
            if opcode == 1: # add
                a = self.read_param_1()
                b = self.read_param_2()
                self.poke(self.address_from_param_3(), a + b)
                self.ip += 4
            elif opcode == 2: # mul
                a = self.read_param_1()
                b = self.read_param_2()
                self.poke(self.address_from_param_3(), a * b)
                self.ip += 4
            elif opcode == 3: # input
                if self.inputs:
                    self.poke(self.address_from_param_1(), self.inputs.pop(0))
                    self.ip += 2
                else:
                    self.state = ComputerState.waiting_for_input
                    break
            elif opcode == 4: # output
                a = self.read_param_1()
                self.outputs.append(a)
                self.ip += 2
            elif opcode == 5: # jump if true
                a = self.read_param_1()
                b = self.read_param_2()
                if a == 0:
                    self.ip += 3
                else:
                    self.ip = b
            elif opcode == 6: # jump if false
                a = self.read_param_1()
                b = self.read_param_2()
                if a == 0:
                    self.ip = b
                else:
                    self.ip += 3
            elif opcode == 7: # less than
                a = self.read_param_1()
                b = self.read_param_2()
                self.poke(self.address_from_param_3(), 1 if a < b else 0)
                self.ip += 4
            elif opcode == 8: # equals
                a = self.read_param_1()
                b = self.read_param_2()
                self.poke(self.address_from_param_3(), 1 if a == b else 0)
                self.ip += 4
            elif opcode == 9: # add to relative base
                a = self.read_param_1()
                self.relative_base += a
                self.ip += 2
            elif opcode == 99: # halt
                self.state = ComputerState.halted
                break
            else:
                raise Exception("Bad opcode {}".format(opcode))


file_path = sys.argv[1]
init_mem = []
with io.open(file_path, "r") as f:
    line = f.readline()
    if line:
        init_mem = [int(x.strip()) for x in line.split(",")]
    else:
        raise Exception("No program in file!")

computer = IntCodeComputer(init_mem, [2])
computer.run()
print("Output: {}\n".format(computer.outputs))
