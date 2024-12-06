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
dirs = [up, right, down, left]

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def clamp(p, mx, my):
    return (min(mx - 1, max(p[0], 0)), min(my - 1, max(p[1], 0)))

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

rocks = set()
y = 0
x = 0

for line in f.readlines():
    line = line.strip()
    if line == "":
        break
    x = 0
    for c in line:
        if c == "#":
            rocks.add((x, y))
        elif c == "^":
            curr = (x, y)
        x += 1
    y += 1

mx = x
my = y

cdir = 0
visited = set()
while clamp(curr, mx, my) == curr:
    next = padd(curr, dirs[cdir])
    if next in rocks:
        cdir = (cdir + 1) % 4
    else:
        visited.add(curr)
        curr = next
print(len(visited))