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

def scan(rows, skip=None):
    for y in range(1, len(rows)):
        if y == skip:
            continue
        dist = min(y, len(rows) - y)
        match = True
        for d in range(1, dist+1):
            if rows[y - d] != rows[y + d - 1]:
                match = False
                break
        if match:
            return y
    return None

def value(rows, skiprow=None, skipcol=None):
    rowres = scan(rows, skiprow)
    if rowres is not None:
        return 100 * rowres
    cols = [[row[i] for row in rows] for i in range(len(rows[0]))]    
    colres = scan(cols, skipcol)
    if colres is not None:
        return colres
    return 0

def value2(rows):
    init = value(rows)
    if init % 100 == 0:
        skiprow = init / 100
        skipcol = None
    else:
        skipcol = init
        skiprow = None
    print(f"init value {init}")
    for y in range(len(rows)):
        for x in range(len(rows[0])):
            if rows[y][x] == "#":
                rows[y][x] = "."
            else:
                rows[y][x] = "#"
            v = value(rows, skiprow, skipcol) 
            """
            if x == 10 and y == 6:
                print(f"v={v}")
                print("\n".join(["".join(row) for row in rows]))
                print()
            """
            if v != 0 and v != init:
                print(f"smudge at {x},{y}, value {v}")
                return v
            if rows[y][x] == "#":
                rows[y][x] = "."
            else:
                rows[y][x] = "#"
    print("\n".join(["".join(row) for row in rows]))

total = 0
for patt in patterns:
    rows = [list(row) for row in patt.strip().splitlines()]
    total += value2(rows)
print(total)