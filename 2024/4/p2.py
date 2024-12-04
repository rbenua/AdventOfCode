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

def pmul(a, b):
    return (a[0] * b[0], a[1] * b[1])

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

dirs = [(-1,1),(-1,-1),(1,-1),(1,1)]

rows = list(f.read().strip().split("\n"))

mx = len(rows[0])
my = len(rows)

def get(p):
    return rows[p[1]][p[0]]

def check(x, y):
    p = (x, y)
    if get(p) != "A":
        return False
    for dir in range(4):
        found = True
        for i in range(4):
            if get(padd(p, dirs[(dir + i) % 4])) != "MMSS"[i]:
                found = False
        if found:
            return True
    return False

total = 0
for y in range(1, my - 1):
    for x in range(1, mx - 1):
        if check(x, y):
            total += 1
print(total)