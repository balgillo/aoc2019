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

    @staticmethod
    def param_1_mode(cmd):
        return ParamMode.from_int((cmd / 100) % 10)

    @staticmethod
    def param_2_mode(cmd):
        return ParamMode.from_int((cmd / 1000) % 10)


class IntCodeComputer:
    def __init__(self, init_mem, input_function, output_function):
        # copy memory using slice
        self.mem = init_mem[:]
        self.input_function = input_function
        self.input_index = 0
        self.output_function = output_function
        self.output_index = 0

    def read(self, index, mode):
        if mode == ParamMode.indirect:
            return self.mem[self.mem[index]]
        elif mode == ParamMode.direct:
            return self.mem[index]
        else:
            raise Exception("Bad read mode {}".format(mode))

    def run(self):
        ip = 0
        while True:
            cmd = self.mem[ip]
            opcode = cmd % 100
            if opcode == 1: # add
                a = self.read(ip + 1, ParamMode.param_1_mode(cmd))
                b = self.read(ip + 2, ParamMode.param_2_mode(cmd))
                self.mem[self.mem[ip + 3]] = a + b
                ip += 4
            elif opcode == 2: # mul
                a = self.read(ip + 1, ParamMode.param_1_mode(cmd))
                b = self.read(ip + 2, ParamMode.param_2_mode(cmd))
                self.mem[self.mem[ip + 3]] = a * b
                ip += 4
            elif opcode == 3: # input
                self.mem[self.mem[ip + 1]] = self.input_function(self.input_index)
                self.input_index += 1
                ip += 2
            elif opcode == 4: # output
                a = self.read(ip + 1, ParamMode.param_1_mode(cmd))
                self.output_function(self.output_index, a)
                self.output_index += 1
                ip += 2
            elif opcode == 5: # jump if true
                a = self.read(ip + 1, ParamMode.param_1_mode(cmd))
                b = self.read(ip + 2, ParamMode.param_2_mode(cmd))
                if a == 0:
                    ip += 3
                else:
                    ip = b
            elif opcode == 6: # jump if false
                a = self.read(ip + 1, ParamMode.param_1_mode(cmd))
                b = self.read(ip + 2, ParamMode.param_2_mode(cmd))
                if a == 0:
                    ip = b
                else:
                    ip += 3
            elif opcode == 7: # less than
                a = self.read(ip + 1, ParamMode.param_1_mode(cmd))
                b = self.read(ip + 2, ParamMode.param_2_mode(cmd))
                self.mem[self.mem[ip + 3]] = 1 if a < b else 0
                ip += 4
            elif opcode == 8: # equals
                a = self.read(ip + 1, ParamMode.param_1_mode(cmd))
                b = self.read(ip + 2, ParamMode.param_2_mode(cmd))
                self.mem[self.mem[ip + 3]] = 1 if a == b else 0
                ip += 4
            elif opcode == 99: # halt
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


def permutations(items):
    return PermutationGenerator(items)


file_path = sys.argv[1]
init_mem = []
with io.open(file_path, "r") as f:
    line = f.readline()
    if line:
        init_mem = [int(x.strip()) for x in line.split(",")]
    else:
        raise Exception("No program in file!")


highest_output = 0

for amp_phases in permutations([0, 1, 2, 3, 4]):
    amp_output = 0
    for amp_phase in amp_phases:
        output = []
        input_function = lambda i: amp_phase if i == 0 else amp_output
        output_function = lambda i, v: output.append(v)
        computer = IntCodeComputer(init_mem, input_function, output_function)
        computer.run()
        amp_output = output[0]
    if amp_output > highest_output:
        highest_output = amp_output

print("Highest output: {}\n".format(highest_output))
