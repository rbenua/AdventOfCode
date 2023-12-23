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

def right_turn(d1, d2):
    return (d1 == up and d2 == right) \
        or (d1 == right and d2 == down) \
        or (d1 == down and d2 == left) \
        or (d1 == left and d2 == up)

dirchars1 = {"U": up, "D": down, "L": left, "R": right}
dirchars2 = {"0": right, "1": down, "2": left, "3": up}

plan = []
for line in f:
    sp = line.split()
    plan.append((dirchars2[sp[2][-2]], int(sp[2][2:-2], base=16)))
    #plan.append((dirchars1[sp[0]], int(sp[1])))

curr = (0, 0)
ranges = []
for i in range(len(plan)):
    (dir, dist) = plan[i]
    nextdir = plan[(i+1) % len(plan)][0]
    prevdir = plan[(i-1) % len(plan)][0]
    if dir in [up, down]:
        dist -= 1
        if right_turn(dir, nextdir):
            dist += 1
        if right_turn(prevdir, dir):
            dist += 1
        ranges.append((curr[0] + (1 if dir == down else 0), dist, dir))
    curr = padd(curr, pmul(dir, dist))
ranges.sort()
print("\n".join(str(r) for r in ranges))

height = 0
prevx = None
total = 0
for (x, dist, dir) in ranges:
    if prevx is not None:
        add = height * (x - prevx)
        if add != 0:
            print("adding ", add)
        total += add
    prevx = x
    print(x, dist, dir)
    if dir == up:
        height += dist
    elif dir == down:
        height -= dist

print(total)