#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
import random

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

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

nodes = set()
edges = {}

for line in f:
    src, rest = line.split(":")
    rest = rest.strip().split()
    nodes.add(src)
    nodes |= set(rest)
    edges |= {(min(src, r), max(src, r)): 1 for r in rest}

print(len(nodes), len(edges))

while True:
    n = nodes.copy()
    e = edges.copy()
    while len(e) > 1:
        rem = random.choice(list(e.keys()))
        e.pop(rem)
        (n1, n2) = rem
        n.difference_update(rem)
        new = n1 + n2
        for other in n:
            amt = 0
            if (min(other, n1), max(other, n1)) in e:
                amt += e.pop((min(other, n1), max(other, n1)))
            if (min(other, n2), max(other, n2)) in e:
                amt += e.pop((min(other, n2), max(other, n2)))
            if amt != 0:
                e[(min(other, new), max(other, new))] = amt
        n.add(new)
    ((n1, n2), count) = e.popitem()
    print(f"contracted to count={count}")
    if count == 3:
        s1 = len(n1) // 3
        s2 = len(n2) // 3
        print(s1 * s2)
        break