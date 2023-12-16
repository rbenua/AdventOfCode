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

grid = [line.strip() for line in f.readlines()]
seen = defaultdict(lambda: set())

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

def step(coords, dir):
    if coords[0] < 0 or coords[0] >= len(grid[0]) or coords[1] < 0 or coords[1] >= len(grid):
        return []
    if dir in seen[coords]:
        return []
    seen[coords].add(dir)

    sym = grid[coords[1]][coords[0]]
    if sym == ".":
        outs = [dir]
    elif sym == "|":
        if dir[0] == 0:
            outs = [dir]
        else:
            outs = [up, down]
    elif sym == "-":
        if dir[1] == 0:
            outs = [dir]
        else:
            outs = [left, right]
    elif sym == "/":
        outs = [pmul((dir[1], dir[0]), -1)]
    elif sym == '\\':
        outs = [(dir[1], dir[0])]
    return [(padd(coords, out), out) for out in outs]

def propagate(coords, dir):
    to_run = deque([(coords, dir)])
    while len(to_run) != 0:
        l = to_run.popleft()
        to_run.extend(step(*l))

propagate((0, 0), right)

for y in range(len(grid)):
    print("".join(['#' if (x, y) in seen else '.' for x in range(len(grid[0]))]))

print(len(seen.keys()))