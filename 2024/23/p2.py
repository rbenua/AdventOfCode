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

def bounds_check(p, mx, my):
    return p[0] >= 0 and p[0] < mx and p[1] >= 0 and p[1] < my

nodes = set()
edges = defaultdict(lambda:set())
for line in f:
    if line.strip() == "":
        continue
    [a, b] = line.strip().split("-")
    nodes.add(a)
    nodes.add(b)
    edges[a].add(b)
    edges[b].add(a)

cliques = set()
for a in nodes:
    for b in edges[a]:
        for c in edges[a]:
            if b != c and b in edges[c]:
                cliques.add(frozenset((a, b, c)))

nextlevel = cliques
while len(nextlevel) > 0:
    cliques = nextlevel
    nextlevel = set()
    for c in cliques:
        for n in nodes:
            if n not in c and edges[n].issuperset(c):
                nextlevel.add(c.union({n}))

print(",".join(sorted(cliques.pop())))