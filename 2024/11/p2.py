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

state = nums(f.read())

memo = {}
def expand(start, steps):
    r = memo.get((start, steps))
    if r is not None:
        return r
    if steps == 0:
        res = 1
    elif start == 0:
        res = expand(1, steps - 1)
    elif len(str(start)) % 2 == 0:
        mid = len(str(start)) // 2
        left = int(str(start)[:mid])
        right = int(str(start)[mid:])
        res = expand(left, steps - 1) + expand(right, steps - 1)
    else:
        res = expand(start * 2024, steps - 1)
    memo[(start, steps)] = res
    return res

print(sum(expand(start, 75) for start in state))