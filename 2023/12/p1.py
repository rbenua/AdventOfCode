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

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

rows = []

for line in f:
    [map, groups] = line.split(" ")
    rows.append((map, [int(g) for g in groups.strip().split(",")]))

def process(map, groups, prefix=""):
    if len(groups) == 0:
        if "#" not in map:
            print(prefix + "." * len(map))
            res = 1
        else:
            res = 0
        #print(f"process 1 {map}, {groups} = {res}")
        return res
    g = groups[0]
    if len(map) < sum(groups) + len(groups) - 1: 
        res = 0
        #print(f"process 2 {map}, {groups} = {res}")
        return 0
    if '.' in map[0:g] or (len(map) > g and map[g] == "#"):
        if(map[0] == "#"):
            #print(f"process 3b {map}, {groups} = 0")
            return 0
        res = process(map[1:], groups, prefix + ("." if len(map) > g else ""))
        #print(f"process 3 {map}, {groups} = {res}, g={g}")
        return res
    if map[0] == '#':
        res = process(map[g+1:], groups[1:], prefix + "#" * g + ("." if len(map) > g else ""))
        #print(f"process 4 {map}, {groups} = {res}")
        return res
    else:
        res = process(map[g+1:], groups[1:], prefix + "#" * g + ("." if len(map) > g else "")) + process(map[1:], groups, prefix + ".")
        #print(f"process 5 {map}, {groups} = {res}")
        return res

total = 0
for (map, groups) in rows:
    print(f"{map} {groups}")
    res = process(map, groups)
    print(res)
    total += res
print(total)