#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

def nbrs(p, minx, maxx, miny, maxy):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if minx <= x <= maxx and miny <= y <= maxy]

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

dirchars = {"U": up, "D": down, "L": left, "R": right}

plan = []
for line in f:
    sp = line.split()
    plan.append((dirchars[sp[0]], int(sp[1])))

dugsquares = set()

curr = (0, 0)
minx = 0
maxx = 0
miny = 0
maxy = 0
for (dir, dist) in plan:
    for _ in range(dist):
        dugsquares.add(curr)
        curr = padd(curr, dir)
    if minx > curr[0]:
        minx = curr[0]
    if maxx < curr[0]:
        maxx = curr[0]
    if miny > curr[1]:
        miny = curr[1]
    if maxy < curr[1]:
        maxy = curr[1]

start = (0, 0)
for d in dugsquares:
    if d[1] == miny and padd(d, down) not in dugsquares:
        start = padd(d, down)
        break

to_process = deque([start])
while len(to_process) > 0:
    curr = to_process.popleft()
    for new in nbrs(curr, minx, maxx, miny, maxy):
        if new not in dugsquares:
            to_process.append(new)
            dugsquares.add(new)
print(len(dugsquares))