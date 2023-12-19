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

line = f.readline()
while line != "\n":
    [name, rest] = line.split("{")
    rules = []
    rs = rest.strip("}\n").split(",")
    for r in rs[:-1]:
        [cond, dst] = r.split(":")
        rules.append(("xmas".find(cond[0]), cond[1], int(cond[2:]), dst))
    rules.append((0, ">", 0, rs[-1]))
    workflows[name] = rules
    line = f.readline()

def process(workflow, bounds):
    if workflow == "A":
        res = 1
        for (lower, upper) in bounds:
            res *= upper - lower
        return res
    if workflow == "R":
        return 0
    rules = workflows[workflow]
    total = 0
    for (var, op, point, dest) in rules:
        b = bounds.copy()
        (lower, upper) = b[var]
        if op == ">" and upper > point + 1:
            b[var] = (max(lower, point + 1), upper)
            bounds[var] = (lower, max(lower, point + 1))
            total += process(dest, b)
            if bounds[var][0] == bounds[var][1]:
                break
        elif op == "<" and lower < point:
            b[var] = (lower, min(point, upper))
            bounds[var] = (min(point, upper), upper)
            total += process(dest, b)
            if bounds[var][0] == bounds[var][1]:
                break
    return total


print(process("in", [(1,4001) for _ in range(4)]))