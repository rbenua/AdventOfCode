#!/usr/bin/env python3

import sys

points = []
bound = int(sys.argv[2])

with open(sys.argv[1], 'r') as f:
    for line in f:
        x = int(line.split(', ')[0])
        y = int(line.split(', ')[1])
        points.append((x,y))

minx = min(points)[0]
maxx = max(points)[0]
miny = min([y for (x,y) in points])
maxy = max([y for (x,y) in points])

# points along the edges will always have an infinite region, we don't need to consider them as starting points
#filtered = [(x,y) for (x,y) in points if x != minx and x != maxx and y != miny and y != maxy]

def dist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

outside = int(bound / len(points))

count = 0
lastrow = 0
for x in range(minx - outside, maxx + outside):
    for y in range(miny - outside, maxy + outside):
        if sum([dist((x, y), p) for p in points]) < bound:
            count += 1
    #if count != 0 and count == lastrow:
    #    break

print(count)
