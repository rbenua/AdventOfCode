#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict

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

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

workflows = {}
parts = []

line = f.readline()
while line != "\n":
    [name, rest] = line.split("{")
    rules = []
    rs = rest.strip("}\n").split(",")
    for r in rs[:-1]:
        [cond, dst] = r.split(":")
        def cf(x, m, a, s, c=cond):
            return eval(c)
        rules.append((cf, dst))
    rules.append((lambda x, m, a, s: True, rs[-1]))
    workflows[name] = rules
    line = f.readline()

line = f.readline()
while line != "":
    parts.append(tuple(nums(line)))
    line = f.readline()

def process(workflow, part):
    if workflow == "A":
        return True
    if workflow == "R":
        return False
    rules = workflows[workflow]
    for (cf, dest) in rules:
        if cf(*part):
            return process(dest, part)

total = 0
for part in parts:
    if process("in", part):
        total += sum(part)
print(total)