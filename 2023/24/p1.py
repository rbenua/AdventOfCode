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

stones = []
for line in f:
    ns = nums(line)
    stones.append(((ns[0], ns[3]), (ns[1], ns[4])))

"""
x0 + t0*dx0 = x1 + t1*dx1; y0 + t0*dy0 = y1 + t1*dy1

t0 = (x1 - x0 + t1*dx1)/dx0; t0 = (y1 - y0 + t1*dy1)/dy0
t1 * dx1/dx0 + (x1-x0)/dx0 = t1 * dy1/dy0 + (y1-y0)/dy0
t1 * (dx1/dx0 - dy1/dy0) = (y1-y0)/dy0 - (x1-x0)/dx0
t1 = ((y1-y0)/dy0 - (x1-x0)/dx0) / (dx1/dx0 - dy1/dy0)
"""

#start = 7
#end = 27
start = 200000000000000
end = 400000000000000

total = 0
for (i, s0) in enumerate(stones[:-1]):
    for s1 in stones[i+1:]:
        ((x0, dx0), (y0, dy0)) = s0
        ((x1, dx1), (y1, dy1)) = s1
        print()
        print(f"Hailstone A: {x0}, {y0} @ {dx0}, {dy0}")
        print(f"Hailstone B: {x1}, {y1} @ {dx1}, {dy1}")
        if dx1/dx0 == dy1/dy0:
            print("Hailstones' paths are parallel; they never intersect.")
            continue
        t1 = ((y1-y0)/dy0 - (x1-x0)/dx0) / (dx1/dx0 - dy1/dy0)
        t0 = (x1 - x0 + t1*dx1)/dx0
        if t1 < 0 or t0 < 0:
            print(f"Hailstones' paths crossed in the past ({t0}, {t1})")
            continue
        x = x1 + t1*dx1
        y = y1 + t1*dy1
        print(f"Hailstones will cross at x={x}, y={y}", end="")
        if start <= x <= end and start <= y <= end:
            print(", *inside* the test area")
            total += 1
        else:
            print(", outside the test area")
print(total)