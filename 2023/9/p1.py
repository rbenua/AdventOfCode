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

seqs = [nums(line) for line in f]

def down(seq):
    curr = seq
    res = [seq]
    while not all(x == 0 for x in curr):
        new = [curr[i+1] - curr[i] for i in range(len(curr) - 1)]
        curr = new
        res.append(curr)
    return res

def up(ss):
    curr = 0
    for seq in reversed(ss):
        curr = seq[-1] + curr
    return curr

total = 0
for seq in seqs:
    ss = down(seq)
    res = up(ss)
    print(res)
    total += res
print(total)