#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cache
from heapq import *

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

mx = my = 71
stop = 1024
if len(sys.argv) > 2:
    mx = my = int(sys.argv[2]) + 1

steps = [tuple(nums(line)) for line in f if line.strip() != ""]

corrupted = set()

def search():
    to_search = [(0, (0, 0))]
    visited = set()
    end = (mx - 1, my - 1)
    while len(to_search) > 0:
        (cscore, curr) = heappop(to_search)
        if curr == end:
            return cscore
        if curr in visited:
            continue
        visited.add(curr)
        for n in nbrs(curr, mx, my):
            if n not in corrupted:
                heappush(to_search, (cscore + 1, n))
    return -1

def printgrid():
    for y in range(my):
        for x in range(mx):
            if (x, y) in corrupted:
                print("#", end="")
            else:
                print(".", end="")
        print()

for c in steps:
    corrupted.add(c)
    if search() == -1:
        (x, y) = c
        print(f"{x},{y}")
        break