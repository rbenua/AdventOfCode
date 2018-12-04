#!/usr/bin/env python3

import sys
import re

pat = re.compile('(\d+),(\d+): (\d+)x(\d+)')
counts = {}

with open(sys.argv[1], 'r') as f:
    for line in f.read().splitlines():
        match = pat.search(line)
        if not match:
            print("broken regex on line {}".format(line))
        startx = int(match.group(1))
        starty = int(match.group(2))
        lenx = int(match.group(3))
        leny = int(match.group(4))
        
        for x in range(startx, startx + lenx):
            for y in range(starty, starty + leny):
                counts[(x, y)] = counts.get((x, y), 0) + 1

dupes = 0
for v in counts.values():
    if v > 1:
        dupes += 1

print(dupes) 
