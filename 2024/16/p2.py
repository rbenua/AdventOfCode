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

def search():
    to_search = [(0, start, right, (start, right))]
    visited = {}
    minscore = 1000000000000
    while len(to_search) > 0:
        (cscore, curr, cdir, prev) = heappop(to_search)
        if cscore > minscore:
            result = set()
            for d in dirs:
                if (end, d) in visited:
                    result.update(visited[end, d][1])
            return result
        if curr == end:
            minscore = cscore
        
        if (curr, cdir) in visited:
            (s, existing) = visited[(curr, cdir)]
            if cscore == s:
                existing.update(visited[prev][1])
            continue
        else:
            visited[(curr, cdir)] = (cscore, visited.get(prev, (0, set()))[1].union({curr}))

        forward = padd(curr, cdir)
        if forward not in walls:
            heappush(to_search, (cscore + 1, forward, cdir, (curr, cdir)))
        heappush(to_search, (cscore + 1000, curr, turn(cdir, True), (curr, cdir)))
        heappush(to_search, (cscore + 1000, curr, turn(cdir, False), (curr, cdir)))
    return -1

result = search()
for y in range(my):
    for x in range(mx):
        if (x, y) in walls:
            print("#", end="")
        elif (x, y) in result:
            print("O", end="")
        else:
            print(" ", end="")
    print()
print(len(result))