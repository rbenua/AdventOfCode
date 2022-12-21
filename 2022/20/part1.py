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

init = [(int(line.strip()), False) for line in f]
#print(0, [x for x,y in init])

def mix(l):
    res = l.copy()
    i = 0
    while i < len(res):
        if res[i][1]:
            i += 1
            continue
        delta = res.pop(i)[0]
        nl = (i + delta) % len(res)
        res.insert(nl, (delta, True))
        if nl <= i:
            i += 1
        #print(i, [x for x,y in res])
    #print("")
    return res

res = mix(init)

#print(res)
z = res.index((0, True))
coords = (res[(z + 1000) % len(res)][0], res[(z + 2000) % len(res)][0], res[(z + 3000) % len(res)][0])
print(coords)
print(sum(coords))
