#!/usr/bin/env python3

import sys
import re

def check(points):
    # return true if every point has at least one other point adjacent to it (including diagonals
    for (x, y) in points:
        if ((x-1,y-1) not in points and (x,y-1) not in points 
        and (x+1,y-1) not in points and (x-1,y) not in points
        and (x+1,y) not in points and (x-1,y+1) not in points
        and (x,y+1) not in points and (x+1,y+1) not in points):
            return False
    return True

def step(ps, vs):
    newpoints = set()
    for i in range(len(ps)):
        (x, y) = ps[i]
        (dx, dy) = vs[i]
        newpoints.add((x+dx, y+dy))
        ps[i] = ((x+dx, y+dy))
    return ps, newpoints 

def printpts(points):
    print(len(points))
    xs = [x for (x,y) in points]
    ys = [y for (x,y) in points]
    minx = min(xs)
    maxx = max(xs)
    miny = min(ys)
    maxy = max(ys)
    for y in range(miny, maxy + 1):
        for x in range(minx, maxx + 1):
            if (x,y) in points:
                print('#', end='')
            else:
                print(".", end='')
        print()

pat = re.compile('position=< ?(-?\d+),  ?(-?\d+)> velocity=< ?(-?\d+),  ?(-?\d+)>')

points = set()
ps = []
vs = []

with open(sys.argv[1], 'r') as f:
    for line in f:
        match = pat.match(line)
        if not match:
            print("could not match ", line)
            exit(1)
        x = int(match.group(1))
        y = int(match.group(2))
        dx = int(match.group(3))
        dy = int(match.group(4))
        points.add((x, y))
        ps.append((x, y))
        vs.append((dx, dy))

stepct = 0
while not check(points):
    stepct += 1
    (ps, points) = step(ps, vs)
    if stepct % 10 == 0:
        print("step ", stepct)
printpts(points)
print(stepct)
