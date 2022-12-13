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

start = (0,0)
end = (0,0)
hts = []
def ht(c, x, y):
    global start, end
    if c == 'S':
        start = (x, y)
        return 0
    elif c == 'E':
        end = (x, y)
        return ord('z') - ord('a')
    else:
        return ord(c) - ord('a')

for (y, line) in enumerate(f):
    hts.append([ht(c, x, y) for (x, c) in enumerate(line.strip())])

visited = set()
to_visit = deque([(0, start)])
while len(to_visit) != 0:
    (dist, p) = to_visit.popleft()
    (px, py) = p
    #print("visiting", dist, p, "height", hts[py][px], to_visit)
    for (nx, ny) in nbrs(p, len(hts[0]), len(hts)):
        if hts[ny][nx] <= hts[py][px] + 1:
            if (nx, ny) == end:
                print(dist + 1)
                exit(0)
            elif (nx, ny) not in visited:
                #print("adding", dist + 1, (nx, ny), "height", hts[ny][nx])
                to_visit.append((dist + 1, (nx, ny)))
                visited.add((nx, ny))

print("not found??")
