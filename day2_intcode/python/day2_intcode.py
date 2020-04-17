#!/bin/python

import sys

mem = [int(n) for n in sys.argv[1].split(",")]
ip = 0
while True:
    cmd = mem[ip]
    if cmd == 1:
        a = mem[mem[ip + 1]]
        b = mem[mem[ip + 2]]
        mem[mem[ip + 3]] = a + b
        ip += 4
    elif cmd == 2:
        a = mem[mem[ip + 1]]
        b = mem[mem[ip + 2]]
        mem[mem[ip + 3]] = a * b
        ip += 4
    elif cmd == 99:
        break
    else:
        exit(1)

print(mem)        
