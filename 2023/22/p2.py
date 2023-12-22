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
    return (a[0] + b[0], a[1] + b[1], a[2] + b[2])

def pmul(a, n):
    return (a[0] * n, a[1] * n, a[2] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

blocks = []
for line in f:
    n = nums(line)
    first = tuple(n[:3])
    second = tuple(n[3:])
    smaller = first if first[2] < second[2] else second
    bigger = first if first[2] >= second[2] else second
    blocks.append((smaller, bigger))

blocks.sort(key=lambda b: b[0][2])

settled = {}
for (i, (b1, b2)) in enumerate(blocks):
    d = padd(b2, pmul(b1, -1))
    blength = max(abs(di) for di in d)
    if blength == 0:
        dir = (0, 0, 0)
    else:
        dir = (int(d[0]/blength), int(d[1]/blength), int(d[2]/blength))

    #print(f"{i}: {b1}~{b2} is {b1} + {dir} * {blength} is ", end="")
    nz = 1
    curr = b1
    for _ in range(blength + 1):
        #print(curr, end=", ")
        for z in reversed(range(1, curr[2])):
            if (curr[0], curr[1], z) in settled and z >= nz:
                nz = z + 1
                break
        curr = padd(curr, dir)
    #print()
    #print("settles to ", end="")
    curr = (b1[0], b1[1], nz)
    for _ in range(blength + 1):
        #print(curr, end=", ")
        settled[curr] = i
        curr = padd(curr, dir)
    #print(curr)
    #print()

#print(settled)
deps = defaultdict(lambda: set())
for (loc, idx) in settled.items():
    if loc[2] == 1:
        deps[idx].add(-1)
    down = padd(loc, (0,0,-1))
    if down in settled:
        next = settled[down]
        if next != idx:
            deps[idx].add(next)

#print(deps)
def chain(start):
    deleted = {start}
    for i in range(start+1, len(blocks)):
        if len(deps[i] - deleted) == 0 and blocks[i][0][2] != 0:
            deleted.add(i)
    
    #print(f"deleting {start} deletes {deleted}")
    return len(deleted) - 1

total = 0
for i in range(len(blocks)):
    res = chain(i)
    total += res
print(total)