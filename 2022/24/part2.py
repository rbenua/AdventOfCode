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

dirs = {"^":(0,-1), ">":(1,0), "v":(0,1), "<":(-1,0)}

lines = f.read().split("\n")
maxy = len(lines) - 3
maxx = len(lines[0]) - 2
print(maxx, maxy)

blizzards = defaultdict(lambda: [])
for (y, line) in enumerate(lines[1:maxy+1]):
    for (x, c) in enumerate(line[1:]):
        if c in dirs:
            blizzards[(x,y)] = [dirs[c]]

def advance(blizzards):
    bs = defaultdict(lambda: [])
    for ((x, y), dirs) in blizzards.items():
        for dir in dirs:
            bs[pmod(padd((x, y), dir), maxx, maxy)].append(dir)
    return bs

def search(start, goal):
    global blizzards
    init_state = (0, start)
    curr_time = 0
    to_visit = deque([(0, start, [])])
    visited = {init_state}
    
    while len(to_visit) > 0:
        (ct, cpos, hist) = to_visit.popleft()
        if ct > curr_time:
            blizzards = advance(blizzards)
            curr_time = ct
        for np in nbrs(cpos, maxx, maxy) + [cpos]:
            if len(blizzards[np]) == 0:
                if np == goal:
                    #print(hist + [np])
                    blizzards = advance(blizzards)
                    return ct + 1
                ns = (ct + 1, np)
                if ns not in visited:
                    visited.add(ns)
                    to_visit.append((ct + 1, np, hist + [np]))


first = search((0, -1), (maxx-1, maxy-1)) 
back = search((maxx-1,maxy), (0,0)) 
second = search((0, -1), (maxx-1, maxy-1))
print(first, back, second)
print(first + back + second)
