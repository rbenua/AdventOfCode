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

total = 0
curr_count = 1
pops = []

for (x, line) in enumerate(f):
    idx = x + 1
    line = line.split(":")[1]
    [ws, hs] = line.split("|")
    want = set(nums(ws))
    matches = 0
    for have in nums(hs):
        if have in want:
            matches += 1
    print(f"{curr_count} of card {idx}")
    total += curr_count
    if matches > 0:
        heapq.heappush(pops, (idx + matches, curr_count))
        curr_count *= 2
    while len(pops) > 0 and pops[0][0] == idx:
        curr_count -= heapq.heappop(pops)[1]
    
print(total)
    