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

walls = set()
x = 0
y = 0
for line in f.read().strip().split("\n"):
    x = 0
    for c in line.strip():
        if c == "#":
            walls.add((x, y))
        elif c == "S":
            start = (x, y)
        elif c == "E":
            end = (x, y)
        x += 1
    y += 1

mx = x
my = y

def search(start):
    to_search = deque([(0, start)])
    visited = {}
    while len(to_search) > 0:
        (cscore, curr) = to_search.popleft()
        if curr in visited:
            continue
        visited[curr] = cscore
        for n in nbrs(curr, mx, my):
            if n not in walls:
                to_search.append((cscore + 1, n))
    return visited

start_dists = search(start)
end_dists = search(end)

goal = start_dists[end] - 100

total = 0

for y1 in range(my):
    for x1 in range(mx):
        p1 = (x1, y1)
        if p1 in walls:
            continue
        for dy in range(-20, 21):
            mdx = 20 - abs(dy)
            for dx in range(-mdx, mdx + 1):
                p2 = padd(p1, (dx, dy))
                if p2 == p1 or clamp(p2, mx, my) != p2 or p2 in walls:
                    continue
                if start_dists[p1] + abs(dx) + abs(dy) + end_dists[p2] <= goal:
                    total += 1

print(total)