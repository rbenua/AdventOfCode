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

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

init = list(enumerate(int(line.strip()) * 811589153 for line in f))

def mix(l):
    for i in range(len(l)):
        val = init[i]
        idx = l.index(val)
        l.pop(idx)
        nl = (idx + val[1]) % len(l)
        l.insert(nl, val)
        #print(i, l)

res = init.copy()
for _ in range(10):
    mix(res)

z = -1
for i in range(len(res)):
    if res[i][1] == 0:
        z = i
        break
#print(z)
coords = (res[(z + 1000) % len(res)][1], res[(z + 2000) % len(res)][1], res[(z + 3000) % len(res)][1])
print(coords)
print(sum(coords))
