#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict

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

def clamp(p, mx, my):
    return (min(mx - 1, max(p[0], 0)), min(my - 1, max(p[1], 0)))

files = []
holes = []
cursor = 0
hole = False
id = 0
for curr in f.read().strip():
    c = int(curr)
    if hole:
        holes.append([cursor, c])
    else:
        files.append([cursor, c, id])
        id += 1
    cursor += c
    hole = not hole

for f in reversed(files):
    [fc, flen, fid] = f
    for hi in range(len(holes)):
        [hc, hlen] = holes[hi]
        if hc > fc:
            break
        if flen <= hlen:
            found = True
            f[0] = hc
            if flen == hlen:
                del holes[hi]
            else:
                holes[hi][0] += flen
                holes[hi][1] -= flen
            break
        
def geom(x):
    return (x * (x + 1)) // 2
total = 0
for [cursor, len, id] in files:
    new = (geom(cursor + len - 1) - geom(cursor-1))
    total += id * new
    cursor += len
print(total)