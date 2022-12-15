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

def closer(p, s):
    return mdist(p, s[0]) <= mdist(s[1], s[0])

def cover_range(y, sensor):
    (s, b) = sensor
    ydist = abs(y - s[1])
    bound = mdist(s, b)
    xdist = bound - ydist
    if xdist < 0:
        return None
    return (s[0] - xdist, s[0] + xdist)

def test_ranges(rs):
    rs.sort(key=lambda x:x[0])
    extent = 0
    for (l, u) in rs:
        if l > extent:
            return l - 1
        extent = max(extent, u)
    if extent < 4000000:
        return extent + 1
    return None

def test_row(y):
    rs = [x for x in [cover_range(y, s) for s in sensors] if x is not None]
    return test_ranges(rs)

delta = 0
while delta <= 2000001:
    delta *= -1
    if delta >= 0:
        delta += 1
    y = 2000000 + delta
    x = test_row(y)
    if x is not None:
        print(4000000 * x + y)
        break
