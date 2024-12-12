#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cache

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

def clamp(p, mx, my):
    return (min(mx - 1, max(p[0], 0)), min(my - 1, max(p[1], 0)))

map = {}
for (y, line) in enumerate(f.read().strip().split("\n")):
    for (x, c) in enumerate(line.strip()):
        map[(x, y)] = c
        mx = x
    my = y

mx += 1
my += 1

visited = set()
def search(p, c):
    to_visit = {p}
    perimeter = 0
    area = 0
    while len(to_visit) > 0:
        curr = to_visit.pop()
        if curr in visited:
            continue
        area += 1
        visited.add(curr)
        for di in range(4):
            dir = dirs[di]
            new = padd(curr, dir)
            if map.get(new) != c:
                rdir = dirs[(di + 1) % 4]
                if map.get(padd(curr, rdir)) != c \
                or map.get(padd(new, rdir)) == c:
                    perimeter += 1
            else:
                to_visit.add(new)
    return area * perimeter

total = 0
for y in range(my):
    for x in range(mx):
        if (x, y) not in visited:
            total += search((x, y), map[(x, y)])
print(total)