#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
import functools

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

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

cubes = set()
for line in f:
    cubes.add(tuple(int(c) for c in line.strip().split(',')))

outside = (min(c[0] for c in cubes) - 1, min(c[1] for c in cubes), min(c[2] for c in cubes))

visit_cache = {outside:True}
def reachable(c):
    visited = {c}
    to_visit = deque([c])
    while len(to_visit) > 0:
        curr = to_visit.popleft()
        if curr in visit_cache:
            r = visit_cache[curr]
            for v in visited:
                visit_cache[v] = r
            return r
        for n in cnbrs(curr):
            if n not in visited and n not in cubes:
                visited.add(n)
                to_visit.append(n)
    #didn't find the outside
    for v in visited:
        visit_cache[v] = False
    return False

total = 0
for c in cubes:
    total += sum(1 if n not in cubes and reachable(n) else 0 for n in cnbrs(c))
print(total)
