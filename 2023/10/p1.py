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

adj = {}
start = (0, 0)

for (y, line) in enumerate(f):
    for (x, c) in enumerate(line):
        if c == 'S':
            start = (x, y)
        elif c == '|':
            adj[(x, y)] = [(x, y+1), (x, y-1)]
        elif c == '-':
            adj[(x, y)] = [(x-1, y), (x+1, y)]
        elif c == 'L':
            adj[(x, y)] = [(x, y-1), (x+1, y)]
        elif c == 'J':
            adj[(x, y)] = [(x, y-1), (x-1, y)]
        elif c == '7':
            adj[(x, y)] = [(x, y+1), (x-1, y)]
        elif c == 'F':
            adj[(x, y)] = [(x, y+1), (x+1, y)]

adj[start] = []
for (k, v) in adj.items():
    if start in v:
        adj[start].append(k)

seen = set()
to_process = deque([(0, start)])
curr_dist = 0
while len(to_process) > 0:
    (curr_dist, curr) = to_process.popleft()
    seen.add(curr)
    for new in adj[curr]:
        if new not in seen:
            to_process.append((curr_dist + 1, new))
print(curr_dist)