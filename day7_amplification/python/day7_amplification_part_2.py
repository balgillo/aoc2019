#!/bin/python

import sys
import io
from enum import Enum



class ParamMode(Enum):
    indirect = 0
    direct = 1

    @staticmethod
    def from_int(x):
        if x == 0:
            return ParamMode.indirect
        elif x == 1:
            return ParamMode.direct
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
        self.inputs = inputs
        self.outputs = []
        self.state = ComputerState.ready

    def read(self, index, mode):
        if mode == ParamMode.indirect:
            return self.mem[self.mem[index]]
        elif mode == ParamMode.direct:
            return self.mem[index]
        else:
            raise Exception("Bad read mode {}".format(mode))

    def read_param_1(self):
        return self.read(self.ip + 1, ParamMode.from_int((self.mem[self.ip] / 100) % 10))

    def read_param_2(self):
        return self.read(self.ip + 2, ParamMode.from_int((self.mem[self.ip] / 1000) % 10))

    def run(self):
        self.state = ComputerState.ready
        self.outputs = []
        while True:
            opcode = self.mem[self.ip] % 100
            if opcode == 1: # add
                a = self.read_param_1()
                b = self.read_param_2()
                self.mem[self.mem[self.ip + 3]] = a + b
                self.ip += 4
            elif opcode == 2: # mul
                a = self.read_param_1()
                b = self.read_param_2()
                self.mem[self.mem[self.ip + 3]] = a * b
                self.ip += 4
            elif opcode == 3: # input
                if self.inputs:
                    self.mem[self.mem[self.ip + 1]] = self.inputs.pop(0)
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
                self.mem[self.mem[self.ip + 3]] = 1 if a < b else 0
                self.ip += 4
            elif opcode == 8: # equals
                a = self.read_param_1()
                b = self.read_param_2()
                self.mem[self.mem[self.ip + 3]] = 1 if a == b else 0
                self.ip += 4
            elif opcode == 99: # halt
                self.state = ComputerState.halted
                break
            else:
                raise Exception("Bad opcode {}".format(opcode))


class PermutationGenerator:
    def __init__(self, items):
        self.items = items
        # self.lehmer_code is a factoradic number ([0] = 1s digit, [1] = 2s digit etc.) that can be turned into
        # a permutation of self.items via the "Lehmer code"
        # See https://en.wikipedia.org/wiki/Factorial_number_system#Permutations
        self.lehmer_code = [0 for i in range(0, len(self.items))]
    
    def __iter__(self):
        return self

    def __len__(self):
        return len(self.items)

    def __next__(self):
        if self.lehmer_code == None:
            raise StopIteration
        remaining_items = self.items[:]

        # Generate permutation by Lehmer code
        ret = [remaining_items.pop(i) for i in reversed(self.lehmer_code)]

        # Increment factoradic number
        overflow = True
        for i in range(0, len(self.items)):
            if self.lehmer_code[i] < i:
                self.lehmer_code[i] += 1
                overflow = False
                break
            else:
                self.lehmer_code[i] = 0
        if overflow:
            # stop iterating on next call to next()
            self.lehmer_code = None
        return ret

    # Python 2 compatibility
    def next(self):
        return self.__next__()


file_path = sys.argv[1]
init_mem = []
with io.open(file_path, "r") as f:
    line = f.readline()
    if line:
        init_mem = [int(x.strip()) for x in line.split(",")]
    else:
        raise Exception("No program in file!")


highest_output = 0

for amp_phases in PermutationGenerator([5, 6, 7, 8, 9]):
    amp_output = 0

    # init computers
    computers = []
    for i in range(0, 5):
        computer = IntCodeComputer(init_mem, [amp_phases[i], amp_output])
        computers.append(computer)
        computer.run()
        amp_output = computer.outputs[0]
    
    # run computers until last computer halts
    while computers[-1].state != ComputerState.halted:
        for computer in computers:
            if computer.state == ComputerState.waiting_for_input:
                computer.inputs = [amp_output]
                computer.run()
            amp_output = computer.outputs[0]
    # check whether we've improved the overall output
    if amp_output > highest_output:
        highest_output = amp_output

print("Highest output: {}\n".format(highest_output))
