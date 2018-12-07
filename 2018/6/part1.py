#!/usr/bin/env python3

import sys

points = []

with open(sys.argv[1], 'r') as f:
    for line in f:
        x = int(line.split(', ')[0])
        y = int(line.split(', ')[1])
        points.append((x,y))

minx = min(points)[0]
maxx = max(points)[0]
miny = min([y for (x,y) in points])
maxy = max([y for (x,y) in points])
distbound = maxx - minx + maxy - miny # if you get this far from a point within its area, it's infinite

# points along the edges will always have an infinite region, we don't need to consider them as starting points
filtered = [(x,y) for (x,y) in points if x != minx and x != maxx and y != miny and y != maxy]

def dist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

def is_closest(check, cand):
    d = dist(check, cand)
    for p in points:
        if p != cand and dist(check, p) <= d:
            return False
    return True

def explore(check, cand):
    #print("Checking ", check, " against ", cand)
    if dist(check, cand) > distbound:
        return -9999999
    if not is_closest(check, cand):
        return 0
    children = 1
    if check[0] <= cand[0] and check[1] == cand[1]:
        children += explore((check[0] - 1, check[1]), cand)
    if check[0] >= cand[0] and check[1] == cand[1]:
        children += explore((check[0] + 1, check[1]), cand)
    if check[1] <= cand[1]:
        children += explore((check[0], check[1] - 1), cand)
    if check[1] >= cand[1]:
        children += explore((check[0], check[1] + 1), cand)
    return children

print(max([explore(p, p) for p in filtered]))
