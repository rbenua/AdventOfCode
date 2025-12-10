#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cache
import z3

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

machines = []

for line in f:
    words = line.strip().split(" ")
    buttons = [[int(idx) for idx in word.strip("()").split(",")] for word in words[1:-1]]
    goals = [int(count) for count in words[-1].strip("{}").split(",")]
    machines.append((buttons, goals))

def solve(buttons, goals):
    presses = [z3.Int(f"p{i}") for i in range(len(buttons))]
    o = z3.Optimize()
    for p in presses:
        o.add(p >= 0)
    counts = [[] for _ in range(len(goals))]
    for (b, p) in zip(buttons, presses):
        for i in b:
            counts[i].append(p)
    for (i, count) in enumerate(counts):
        o.add(goals[i] == sum(count))
    total = sum(presses)
    o.minimize(total)
    if(o.check() == z3.sat):
        return o.model().eval(total).as_long()
    else:
        return -1

all = 0
for m in machines:
    r = solve(*m)
    # print(r)
    all += r
print(all)