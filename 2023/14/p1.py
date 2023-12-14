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

allrocks = set()
circles = []

maxy = 0
maxx = 0
for (y, line) in enumerate(f):
    maxy = y + 1
    for (x, c) in enumerate(line.strip()):
        maxx = x + 1
        if c == 'O':
            circles.append((y, x))
            allrocks.add((y, x))
        elif c == "#":
            allrocks.add((y, x))

circles.sort()
print(circles)

total = 0
for (cy, cx) in circles:
    allrocks.remove((cy, cx))
    for y in reversed(range(-1, cy)):
        if (y, cx) in allrocks or y == -1:
            ny = y + 1
            print(f"{cy, cx} goes to {ny, cx}, contributes {maxy - ny}")
            allrocks.add((ny, cx))
            total += maxy - ny
            break
print(total)
