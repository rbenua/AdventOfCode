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

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

sensors = []
beacons = set()
for line in f:
    [sx, sy, bx, by] = nums(line)
    sensors.append(((sx, sy), (bx, by)))
    beacons.add((bx, by))

minx = min([s[1][0] for s in sensors])
maxx = max([s[1][0] for s in sensors])

def closer(p, s):
    return mdist(p, s[0]) <= mdist(s[1], s[0])

total = 0
for x in range(minx, maxx+1):
    if (x, 2000000) not in beacons and any([closer((x, 2000000), s) for s in sensors]):
    #if (x, 11) not in beacons and any([closer((x, 11), s) for s in sensors]):
        total += 1
x = maxx
while True:
    x += 1
    if (x, 2000000) not in beacons and any([closer((x, 2000000), s) for s in sensors]):
    #if (x, 11) not in beacons and any([closer((x, 11), s) for s in sensors]):
        total += 1
    else:
        break
x = minx
while True:
    x -= 1
    if (x, 2000000) not in beacons and any([closer((x, 2000000), s) for s in sensors]):
    #if (x, 11) not in beacons and any([closer((x, 11), s) for s in sensors]):
        total += 1
    else:
        break

print(total)
