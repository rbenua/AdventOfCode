#!/usr/bin/env python3

import sys
import re

pat = re.compile('#(\d+) @ (\d+),(\d+): (\d+)x(\d+)')
counts = {}
claims = []

with open(sys.argv[1], 'r') as f:
    for line in f.read().splitlines():
        match = pat.search(line)
        if not match:
            print("broken regex on line {}".format(line))
        id = int(match.group(1))
        startx = int(match.group(2))
        starty = int(match.group(3))
        lenx = int(match.group(4))
        leny = int(match.group(5))
        claims.append((id, startx, starty, lenx, leny))
        
        for x in range(startx, startx + lenx):
            for y in range(starty, starty + leny):
                counts[(x, y)] = counts.get((x, y), 0) + 1

for (id, startx, starty, lenx, leny) in claims:
    contended = False
    for x in range(startx, startx + lenx):
        for y in range(starty, starty + leny):
            if counts[(x, y)] > 1:
                contended = True
                break
        if contended:
            break
    if not contended:
        print(id)
        exit(0)
