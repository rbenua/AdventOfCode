#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
import heapq

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

def nbrs(p, mx, my):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if 0 <= x < mx and 0 <= y < my]

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)
dirchs = {up: "^", down: "v", left: "<", right: ">"}

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

grid = [[int(c) for c in line.strip()] for line in f]

def search(start, end):
    to_search = [(0, start, None, 10, [])]
    seen = {}
    while len(to_search) > 0:
        (cdist, curr, cdir, cleft, sequence) = heapq.heappop(to_search)
        if curr == end:
            return (cdist, sequence)
        if (curr, cdir) in seen:
            prevdist = seen[(curr, cdir)]
            if prevdist >= cleft:
                continue
        seen[(curr, cdir)] = cleft 
        #print(f"{cdist}: {curr}, {dirchs[cdir]}, {cleft}")
        dirs = [right, down, up, left]
        if cdir is not None:
            dirs.remove(pmul(cdir, -1))
            if cleft == 0:
                dirs.remove(cdir)
        for dir in dirs:
            if dir == cdir:
                rem = cleft - 1
                new = padd(curr, dir)
                path = [(new, dir)]
            else:
                rem = 6
                new = padd(curr, pmul(dir, 4))
                path = [(padd(curr, dir), dir), (padd(curr, pmul(dir, 2)), dir),
                        (padd(curr, pmul(dir, 3)), dir), (new, dir)]
            if new[0] < 0 or new[0] >= len(grid[0]) or new[1] < 0 or new[1] >= len(grid):
                continue
            weight = sum(grid[p[1]][p[0]] for (p, _) in path)
            heapq.heappush(to_search, (cdist + weight, new, dir, rem, sequence + path))

res, seq = search((0,0), (len(grid[0])-1, len(grid)-1))

gridstrs = [[str(x) for x in line] for line in grid]
for (coord, dir) in seq:
    gridstrs[coord[1]][coord[0]] = dirchs[dir]
print("\n".join("".join(line) for line in gridstrs))
print(res)