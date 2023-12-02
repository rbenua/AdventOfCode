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

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

lines = [l.rstrip() for l in f]
insns = deque(lines.pop())
lines.pop()


DIRS = [(1,0),(0,1),(-1,0),(0,-1)]

walls = set()
grid = set()

for (y, line) in enumerate(lines):
    w = set((x, y) for (x, c) in enumerate(line) if c == "#")
    walls |= w
    g = set((x, y) for (x, c) in enumerate(line) if c == '.')
    grid |= w
    grid |= g

facing = 0
pos = (0,0)
while (pos not in grid) or (pos in walls):
    pos = padd(pos, DIRS[facing])

def forward(pos, facing, amt):
    #print("forward", pos, facing, amt, "= ", end="")
    for _ in range(amt):
        np = padd(pos, DIRS[facing])
        if np not in grid:
            rev = pmul(DIRS[facing], -1)
            np = pos
            while np in grid:
                np = padd(np, rev)
            np = padd(np, DIRS[facing])
        if np in walls:
            break
        pos = np
    #print(pos)
    return pos

amt_acc = ""
while len(insns) > 0:
    c = insns.popleft()
    if c.isnumeric():
        amt_acc += c
    elif c == "R":
        if amt_acc != "":
            amt = int(amt_acc)
            pos = forward(pos, facing, amt)
            amt_acc = ""
        facing = (facing + 1) % 4
    elif c == "L":
        if amt_acc != "":
            amt = int(amt_acc)
            pos = forward(pos, facing, amt)
            amt_acc = ""
        facing = (facing - 1) % 4

if amt_acc != "":
    amt = int(amt_acc)
    pos = forward(pos, facing, amt)
    amt_acc = ""

print(pos, facing)
print(1000 * (pos[1] + 1) + 4 * (pos[0] + 1) + facing)
