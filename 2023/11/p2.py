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

gals = []
max_y = 0
for (y, line) in enumerate(f):
    max_y = y + 1
    for (x, c) in enumerate(line.strip()):
        max_x = x + 1
        if c == "#":
            print(x, y)
            gals.append([x, y])

print(gals)

def expand(v, c):
    should_expand = True
    for gal in gals:
        if gal[c] == v:
            should_expand = False
    if should_expand:
        print(v)
        for gal in gals:
            if gal[c] > v:
                gal[c] += 1000000 - 1
print("expanding rows")
for y in reversed(range(max_y)):
    expand(y, 1)
print("expanding cols")
for x in reversed(range(max_x)):
    expand(x, 0)

print(gals)

total = 0
while len(gals) > 0:
    curr = gals.pop()
    for g in gals:
        print(f"{curr} to {g} is {mdist(g, curr)}")
        total += mdist(g, curr)
print(total)