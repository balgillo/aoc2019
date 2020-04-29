#!/bin/python

import sys
from enum import Enum

def read(mem, index, mode):
    if mode == ParamMode.indirect:
        return mem[mem[index]]
    elif mode == ParamMode.direct:
        return mem[index]
    else:
        raise Exception("Bad read mode {}".format(mode))


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


def param_1_mode(cmd):
    return ParamMode.from_int((cmd // 100) % 10)


def param_2_mode(cmd):
    return ParamMode.from_int((cmd // 1000) % 10)


def run(mem):
    ip = 0
    while True:
        cmd = mem[ip]
        opcode = cmd % 100
        if opcode == 1:
            a = read(mem, ip + 1, param_1_mode(cmd))
            b = read(mem, ip + 2, param_2_mode(cmd))
            mem[mem[ip + 3]] = a + b
            ip += 4
        elif opcode == 2:
            a = read(mem, ip + 1, param_1_mode(cmd))
            b = read(mem, ip + 2, param_2_mode(cmd))
            mem[mem[ip + 3]] = a * b
            ip += 4
        elif opcode == 3:
            print("Input: ")
            mem[mem[ip + 1]] = int(input())
            ip += 2
        elif opcode == 4:
            a = read(mem, ip + 1, param_1_mode(cmd))
            print("Output: {}".format(a))
            ip += 2
        elif opcode == 99:
            break
        else:
            raise Exception("Bad opcode {}".format(opcode))

mem = [int(n) for n in sys.argv[1].split(",")]

run(mem)

print("Mem: {}\n".format(mem))
