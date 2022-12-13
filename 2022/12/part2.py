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

end = (0,0)
hts = []
def ht(c, x, y):
    global end
    if c == 'S':
        return 0
    elif c == 'E':
        end = (x, y)
        return ord('z') - ord('a')
    else:
        return ord(c) - ord('a')

for (y, line) in enumerate(f):
    hts.append([ht(c, x, y) for (x, c) in enumerate(line.strip())])

dists = []
for start in [(0, y) for y in range(len(hts))]: 
    visited = set()
    to_visit = deque([(0, start)])
    while len(to_visit) != 0:
        (dist, p) = to_visit.popleft()
        (px, py) = p
        found = -1
        for (nx, ny) in nbrs(p, len(hts[0]), len(hts)):
            if hts[ny][nx] <= hts[py][px] + 1:
                if (nx, ny) == end:
                    found = dist + 1
                    break
                elif (nx, ny) not in visited:
                    to_visit.append((dist + 1, (nx, ny)))
                    visited.add((nx, ny))
        if found > 0:
            dists.append(found)
            break

print(min(dists))

