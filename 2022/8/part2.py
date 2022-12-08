#!/usr/bin/env python3

import sys
import re
f = open(sys.argv[1], "r")

hts = []
for line in f:
    hts.append([int(c) for c in line.strip()])

def probe(target, x, y, dx, dy):
    lx = x + dx
    ly = y + dy
    if not (0 <= lx < len(hts[0]) and 0 <= ly < len(hts)):
        return 0
    elif hts[ly][lx] >= target:
        return 1
    else:
        return 1 + probe(target, lx, ly, dx, dy)

best = 0
for y in range(len(hts)):
    for x in range(len(hts[0])):
        target = hts[y][x]
        score = (probe(target, x, y, 0, 1) * 
                 probe(target, x, y, 0, -1) * 
                 probe(target, x, y, 1, 0) * 
                 probe(target, x, y, -1, 0))
        if score > best:
            best = score
print(best)

