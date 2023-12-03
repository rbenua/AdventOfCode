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

gearlocs = {}

ls = f.readlines()

for (y, line) in enumerate(ls):
    for (x, c) in enumerate(line.strip()):
        if c == "*":
            gearlocs[(x, y)] = (0, 1)

#print(symlocs)
total = 0
for (y, line) in enumerate(ls):
    for m in re.finditer("([\d]+)", line):
        cur = int(m.group(0))
        to_check = [(m.start() - 1, y), (m.end(), y)]
        for x in range(m.start() - 1, m.end() + 1):
            to_check += [(x, y - 1), (x, y + 1)]
        for c in to_check:
            if c in gearlocs:
                (count, prev) = gearlocs[c]
                if count == 0:
                    gearlocs[c] = (1, cur)
                elif count == 1:
                    gearlocs[c] = (2, prev * cur)
                    total += prev * cur
                elif count == 2:
                    gearlocs[c] = (3, 0)
                    total -= prev

print(total)