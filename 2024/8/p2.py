#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from math import gcd

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

nodes = defaultdict(lambda:[])
y = 0
x = 0
for line in f.read().strip().split("\n"):
    x = 0
    for c in line.strip():
        if c != '.':
            nodes[c].append((x, y))
        x += 1
    y += 1

(mx, my) = (x, y)
antinodes = set()

def reduce(delta):
    (dx, dy) = delta
    g = gcd(dx, dy)
    return (dx // g, dy // g)

def project(a, dir):
    while clamp(a, mx, my) == a:
        antinodes.add(a)
        a = padd(a, dir)

for v in nodes.values():
    for (i, fst) in enumerate(v[:-1]):
        for snd in v[i+1:]:
            delta = reduce(psub(snd, fst))
            project(fst, delta)
            project(fst, pmul(delta, -1))
print(len(antinodes))