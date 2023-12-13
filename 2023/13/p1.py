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

patterns = f.read().split("\n\n")
print(len(patterns))

def scan(rows):
    for y in range(1, len(rows)):
        dist = min(y, len(rows) - y)
        match = True
        print(f"y={y}, dist={dist}")
        for d in range(1, dist+1):
            print(f"comparing {y-d} to {y+d-1}, {rows[y-d]} | {rows[y+d-1]}")
            if rows[y - d] != rows[y + d - 1]:
                match = False
                break
        if match:
            return y
    return None

def value(pattern):
    rows = pattern.strip().splitlines()
    rowres = scan(rows)
    if rowres is not None:
        print(f"row match {rowres}")
        return 100 * rowres
    cols = [[row[i] for row in rows] for i in range(len(rows[0]))]    
    print("\n".join(rows))
    print()
    print("\n".join(["".join(col) for col in cols]))
    colres = scan(cols)
    if colres is not None:
        print(f"col match {colres}")
        return colres
    return 0

total = 0
for patt in patterns:
    total += value(patt)
print(total)