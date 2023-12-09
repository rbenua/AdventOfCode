#!/usr/bin/env python3

import sys
import re
import itertools
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

map = {}
reg = re.compile("(\w{3}) = \((\w{3}), (\w{3})\)")

dirs = f.readline().strip()

for line in f:
    m = reg.match(line)
    if m is not None:
        map[m.group(1)] = (m.group(2), m.group(3))

total = 0
curr = "AAA"
for dir in itertools.cycle(dirs):
    #print(total, dir, curr)
    if dir == "L":
        curr = map[curr][0]
    else:
        curr = map[curr][1]
    total += 1
    if curr == "ZZZ":
        print(total)
        exit()