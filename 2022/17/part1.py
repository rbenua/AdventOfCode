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

jets = [1 if c == '>' else -1 for c in f.read().strip()]

shapes = [
    [(0, 0), (1, 0), (2, 0), (3, 0)], #-
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)], #+
    [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)], #j
    [(0, 0), (0, 1), (0, 2), (0, 3)], #I
    [(0, 0), (0, 1), (1, 0), (1, 1)] #o
]

jetidx = 0
shapeidx = 0
height = 0
occupied = set()
DOWN = (0, -1)

def testpt(pt, dir):
    (nx, ny) = padd(pt, dir)
    return 0 <= nx < 7 and ny >= 0 and (nx, ny) not in occupied

def testshape(shape, dir):
    return all([testpt(p, dir) for p in shape])

def step():
    global jetidx, shapeidx, height, occupied, DOWN
    shape = [(x + 2, y + height + 3) for (x, y) in shapes[shapeidx]]
    shapeidx = (shapeidx + 1) % len(shapes)
    jetx = jets[jetidx]
    jetidx = (jetidx + 1) % len(jets)
    jetvec = (jetx, 0)
    if testshape(shape, jetvec):
        shape = [padd(p, jetvec) for p in shape]
    while testshape(shape, DOWN):
        shape = [padd(p, DOWN) for p in shape]
        jetx = jets[jetidx]
        jetidx = (jetidx + 1) % len(jets)
        jetvec = (jetx, 0)
        if testshape(shape, jetvec):
            shape = [padd(p, jetvec) for p in shape]
    occupied.update(shape)
    height = max(height, max([y+1 for (x, y) in shape]))

def printgrid():
    for y in range(height, -1, -1):
        print("".join(["#" if (x, y) in occupied else "." for x in range(7)]))
    print()

for _ in range(2022):
    step()
    #printgrid()
print(height)
