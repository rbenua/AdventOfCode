#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cache

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

def nbrs(p, mx, my):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if 0 <= x < mx and 0 <= y < my]

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)
dirs = [up, right, down, left]

def turn(dir, right):
    delta = 1 if right else -1
    return dirs[(dirs.index(dir) + delta) % 4]

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def psub(a, b):
    return (a[0] - b[0], a[1] - b[1])

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

def clamp(p, mx, my):
    return (min(mx - 1, max(p[0], 0)), min(my - 1, max(p[1], 0)))

registers = {"A": 0, "B": 0, "C": 0}
pc = 0
program = []
outputs = []

for line in f:
    if line.startswith("Register"):
        [_, name, value] = line.split(" ")
        registers[name.strip(":")] = int(value)
    elif line.startswith("Program"):
        [_, prog] = line.split(" ")
        program = [int(c) for c in prog.split(",")]

print(registers)
print(program)

def combo(op):
    if op < 4:
        return op
    elif op == 4:
        return registers["A"]
    elif op == 5:
        return registers["B"]
    elif op == 6:
        return registers["C"]
    return None

def adv(op):
    registers["A"] = registers["A"] // (1 << combo(op))

def bxl(op):
    registers["B"] = registers["B"] ^ op

def bst(op):
    registers["B"] = combo(op) % 8

def jnz(op):
    global pc
    if registers["A"] != 0:
        pc = op - 2
    
def bxc(op):
    registers["B"] = registers["B"] ^ registers["C"]

def output(op):
    outputs.append(combo(op) % 8)

def bdv(op):
    registers["B"] = registers["A"] // (1 << combo(op))

def cdv(op):
    registers["C"] = registers["A"] // (1 << combo(op))

ops = [adv, bxl, bst, jnz, bxc, output, bdv, cdv]

def run():
    global pc
    while pc < len(program) - 1:
        code = program[pc]
        op = program[pc+1]
        print(f"{pc}: {ops[code].__name__} {op} - {registers}", end="")
        ops[code](op)
        print(f" -> {registers}")
        pc += 2

run()
print(",".join(str(o) for o in outputs))