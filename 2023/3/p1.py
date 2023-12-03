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

symlocs = set()

ls = f.readlines()

for (y, line) in enumerate(ls):
    for (x, c) in enumerate(line.strip()):
        if not c.isdigit() and c != ".":
            symlocs.add((x, y))

#print(symlocs)
total = 0
for (y, line) in enumerate(ls):
    for m in re.finditer("([\d]+)", line):
        #print(f"group {m.group(0)} at ({m.start()}..{m.end()}, {y})")
        if (m.start() - 1, y) in symlocs or (m.end(), y) in symlocs:
            total += int(m.group(0))
            #print("beside", int(m.group(0)))
            continue
        for x in range(m.start() - 1, m.end() + 1):
            if (x, y - 1) in symlocs or (x, y + 1) in symlocs:
                total += int(m.group(0))
                #print("around", int(m.group(0)))
                break

print(total)