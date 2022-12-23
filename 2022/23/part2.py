#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]
"""
def nbrs(p, mx, my):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if 0 <= x < mx and 0 <= y < my]
"""

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

dirs = deque([((0,-1), [(-1,-1), (0,-1), (1,-1)]),
              ((0, 1), [(-1, 1), (0, 1), (1, 1)]),
              ((-1,0), [(-1,-1), (-1,0), (-1,1)]),
              ((1, 0), [(1, -1), (1, 0), (1, 1)])])

nbrs = [(-1,-1),(-1,0),(-1,1),
        (0, -1),       (0, 1),
        (1, -1),(1, 0),(1, 1)]

elves = set()
for (y, line) in enumerate(f):
    for (x, c) in enumerate(line.rstrip()):
        if c == "#":
            elves.add((x, y))

def propose(e, es):
    if all(padd(e, n) not in es for n in nbrs):
        return e
    for (d, check) in dirs:
        if all(padd(e, n) not in es for n in check):
            return padd(e, d)
    return e

def step(es):
    props = defaultdict(lambda: 0)
    moves = {}
    for e in es:
        p = propose(e, es)
        moves[e] = p
        props[p] += 1
    res = set()
    for e in es:
        if props[moves[e]] == 1:
            res.add(moves[e])
        else:
            res.add(e)
    dirs.append(dirs.popleft())
    return res

def printgrid(es):
    minx = min(x for x,y in es)
    maxx = max(x for x,y in es)
    miny = min(y for x,y in es)
    maxy = max(y for x,y in es)
    for y in range(miny, maxy+1):
        print("".join('#' if (x,y) in es else '.' for x in range(minx, maxx+1)))

r = 0
while True:
    es = step(elves)
    r += 1
    #print(r)
    #printgrid(es)
    if es == elves:
        break
    elves = es

print(r)

