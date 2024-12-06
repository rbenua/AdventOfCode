#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from bisect import bisect, insort

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
dirs = [up, right, down, left]

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def clamp(p, mx, my):
    return (min(mx - 1, max(p[0], 0)), min(my - 1, max(p[1], 0)))

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

rocks = set()
y = 0
x = 0

orig_rows = []
orig_cols = []

for line in f.readlines():
    line = line.strip()
    if line == "":
        break
    x = 0
    row = []
    for c in line:
        if y == 0:
            orig_cols.append([])
        if c == "#":
            rocks.add((x, y))
            row.append(x)
            orig_cols[x].append(y)
        elif c == "^":
            start = (x, y)
        x += 1
    orig_rows.append(row)
    y += 1

mx = x
my = y

def move(pos, dir, rows, cols):
    (px, py) = pos
    if dir == 0 or dir == 2:
        l = cols[px]
        idx = bisect(l, py)
        if dir == 0:
            if idx == 0:
                return None
            return (px, l[idx-1]+1)
        else:
            if idx == len(l):
                return None
            return (px, l[idx]-1)
    else:
        l = rows[py]
        idx = bisect(l, px)
        if dir == 3:
            if idx == 0:
                return None
            return (l[idx-1]+1, py)
        else:
            if idx == len(l):
                return None
            return (l[idx]-1, py)


def check(rows, cols):
    curr = start
    cdir = 0
    visited = set()
    while curr is not None:
        if (curr, cdir) in visited:
            return True
        visited.add((curr, cdir))
        curr = move(curr, cdir, rows, cols)
        cdir = (cdir + 1) % 4
    return False


initial_visits = set()
curr = start
cdir = 0
while clamp(curr, mx, my) == curr:
    next = padd(curr, dirs[cdir])
    if next in rocks:
        cdir = (cdir + 1) % 4
    else:
        initial_visits.add(curr)
        curr = next

total = 0
for (cx, cy) in initial_visits:
    #rows = [row.copy() for row in orig_rows]
    #cols = [col.copy() for col in orig_cols]
    row_idx = bisect(orig_rows[cy], cx)
    orig_rows[cy].insert(row_idx, cx)
    col_idx = bisect(orig_cols[cx], cy)
    orig_cols[cx].insert(col_idx, cy)
    if check(orig_rows, orig_cols):
        total += 1
    del orig_rows[cy][row_idx]
    del orig_cols[cx][col_idx]
    
print(total)