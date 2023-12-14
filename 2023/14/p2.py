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

allrocks = set()
circles = []

maxy = 0
maxx = 0
for (y, line) in enumerate(f):
    maxy = y + 1
    for (x, c) in enumerate(line.strip()):
        maxx = x + 1
        if c == 'O':
            circles.append((y, x))
            allrocks.add((y, x))
        elif c == "#":
            allrocks.add((y, x))

def tilt(dir):
    global circles, allrocks
    circles.sort(key=lambda c: (c[0] * dir[0] * -1, c[1] * dir[1] * -1))
    new_circles = []
    for (cy, cx) in circles:
        allrocks.remove((cy, cx))
        n = (cy, cx)
        while n[1] >= 0 and n[1] < maxx and n[0] >= 0 and n[0] < maxy:
            n = padd(n, dir)
            if n in allrocks:
                break
        n = padd(n, pmul(dir, -1))
        allrocks.add(n)
        new_circles.append(n) 
    circles = new_circles

def cycle():
    tilt((-1, 0))
    tilt((0, -1))
    tilt((1, 0))
    tilt((0, 1))

states = {}
remainder = 0
for step in range(1000000000):
    fc = frozenset(circles)
    if fc in states:
        prev = states[fc]
        jump = step - prev
        remainder = (1000000000 - step) % jump
        print(f"found cycle of length {jump}, skipping to {remainder} left")
        break
    else:
        states[fc] = step
        cycle()

for _ in range(remainder):
    cycle()

total = 0
for (cy, _) in circles:
    total += maxy - cy
print(total)