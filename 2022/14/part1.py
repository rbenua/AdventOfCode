#!/usr/bin/env python3

import sys
import re
from collections import deque

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

def nbrs(p, mx, my):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if 0 <= x < mx and 0 <= y < my]

walls = set()
for line in f:
    ns = nums(line)
    (prevx, prevy) = (ns.pop(0), ns.pop(0))
    while len(ns) > 0:
        (cx, cy) = ns.pop(0), ns.pop(0)
        if cx == prevx:
            walls.update([(prevx, y) for y in range(min(cy, prevy), max(cy, prevy) + 1)])
        else:
            walls.update([(x, prevy) for x in range(min(cx, prevx), max(cx, prevx) + 1)])
        (prevx, prevy) = (cx, cy)

nwalls = len(walls)
maxy = max([y for (x, y) in walls])

done = False
while not done:
    (cx, cy) = (500, 0)
    done = True
    while cy < maxy:
        if (cx, cy + 1) not in walls:
            cy += 1
        elif (cx - 1, cy + 1) not in walls:
            cx -= 1
            cy += 1
        elif (cx + 1, cy + 1) not in walls:
            cx += 1
            cy += 1
        else:
            walls.add((cx, cy))
            done = False
            break
print(len(walls) - nwalls)
