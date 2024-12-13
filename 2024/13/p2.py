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

for block in f.read().strip().split("\n\n"):
    machines.append(tuple(nums(block)))

def cost(a, b, g):
    na = z3.Int('na')
    nb = z3.Int('nb')
    o = z3.Optimize()
    o.add(a[0] * na + b[0] * nb == g[0])
    o.add(a[1] * na + b[1] * nb == g[1])
    cost = 3 * na + nb
    o.minimize(cost)
    if o.check() == z3.sat:
        return o.model().eval(cost).as_long()
    else:
        return 0 

total = 0
for (ax, ay, bx, by, gx, gy) in machines:
    a = (ax, ay)
    b = (bx, by)
    g = padd((gx, gy), (10000000000000, 10000000000000))
    total += cost(a, b, g)
print(total)

# ax * na + bx * nb = gx
# ay * na + by * nb = gy
# 
# na = gx / ax - bx * nb / ax
#
# ay * gx / ax - ay * bx * nb / ax + by * nb = gy 
# gy - aygx/ax = (by - aybx/ax)nb
# nb = (gy - aygx/ax)/(by - aybx/ax)
# nb = (axgy - aygx) / (axby - aybx)
# fuck it, time for z3