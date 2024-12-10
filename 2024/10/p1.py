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

def clamp(p, mx, my):
    return (min(mx - 1, max(p[0], 0)), min(my - 1, max(p[1], 0)))

positions = [set() for _ in range(10)]

x = 0
y = 0
for line in f:
    x = 0
    for c in line.strip():
        if c in "0123456789":
            positions[int(c)].add((x, y))
        x += 1
    y += 1

mx = x
my = y

total = 0
for start in positions[0]:
    height = 0
    frontier = {start}
    while height < 9:
        nf = set()
        for p in frontier:
            for n in nbrs(p, mx, my):
                if n in positions[height + 1]:
                    nf.add(n)
        frontier = nf
        height += 1
    total += len(frontier)
print(total)
