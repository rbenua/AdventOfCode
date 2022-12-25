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

digits = {"2":2, "1":1, "0":0, "-":-1, "=":-2}

def s2d(s):
    total = 0
    for d in s:
        total *= 5
        total += digits[d]
    return total

def d2s(d):
    s = ""
    while d != 0:
        m = d % 5
        d = d // 5
        if m == 3:
            s = "=" + s
            d += 1
        elif m == 4:
            s = "-" + s
            d += 1
        else:
            s = str(m) + s
    return s

total = 0
for line in f:
    d = s2d(line.strip())
    print(line.strip(), d)
    total += d
print(total)
print(d2s(total))
