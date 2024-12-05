#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cmp_to_key

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

input = f.read()
[rules, updates] = input.split("\n\n")

constraints = set()

for rule in rules.split("\n"):
    [lhs, rhs] = rule.strip().split("|")
    constraints.add((lhs, rhs))

total = 0
def pcmp(x, y):
    if (x, y) in constraints:
        return -1
    elif (y, x) in constraints:
        return 1
    else:
        return 0

for update in updates.strip().split("\n"):
    pages = update.strip().split(",")
    correct = True
    for l in range(len(pages) - 1):
        for r in range(l + 1, len(pages)):
            if (pages[r], pages[l]) in constraints:
                correct = False
                break
        if not correct:
            break
    if not correct:
        pages.sort(key=cmp_to_key(pcmp))
        total += int(pages[len(pages) // 2])
print(total)