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
for curr in f.read().strip():
    c = int(curr)
    if hole:
        holes.append([cursor, c])
    else:
        files.append([cursor, c])
    cursor += c
    hole = not hole

hole_idx = 0
file_idx = len(files) - 1
result = [(0, files[0][1])] # [(file id, length)]
while holes[hole_idx][0] < files[file_idx][0]:
    file_size = files[file_idx][1]
    hole_size = holes[hole_idx][1]
    if file_size <= hole_size: # this file fits completely into the current hole
        result.append((file_idx, file_size))
        file_idx -= 1
        holes[hole_idx][1] -= file_size
        holes[hole_idx][0] += file_size
        if holes[hole_idx][1] == 0: # the hole is completely filled by the file
            result.append((hole_idx + 1, files[hole_idx + 1][1])) # put the file after the current hole into result
            hole_idx += 1
    else: # this file needs to be split between the holes
        result.append((file_idx, hole_size))
        files[file_idx][1] -= hole_size # can't be 0 because that would go to the <= side
        result.append((hole_idx + 1, files[hole_idx + 1][1])) # put the file after the current hole into result
        hole_idx += 1

def geom(x):
    return (x * (x + 1)) // 2
cursor = 0
total = 0
for (id, len) in result:
    new = (geom(cursor + len - 1) - geom(cursor-1))
    total += id * new
    cursor += len
print(total)