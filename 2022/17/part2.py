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

jets = [1 if c == '>' else -1 for c in f.read().strip()]

shapes = [
    [(0, 0), (1, 0), (2, 0), (3, 0)], #-
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)], #+
    [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)], #j
    [(0, 0), (0, 1), (0, 2), (0, 3)], #I
    [(0, 0), (0, 1), (1, 0), (1, 1)] #o
]

jetidx = 0
shapeidx = 0
height = 0
occupied = set()
offset = 0
DOWN = (0, -1)
cache = {}

def testpt(pt, dir):
    (nx, ny) = padd(pt, dir)
    return 0 <= nx < 7 and ny >= 0 and (nx, ny) not in occupied

def testshape(shape, dir):
    return all([testpt(p, dir) for p in shape])

def testrows(off):
    for col in range(7):
        if (col, off) not in occupied and (col, off + 1) not in occupied and (col, off + 2) not in occupied:
            return False
    return True

def prune():
    global occupied, offset, height
    pruned = False
    for off in range(height - 2, 0, -1):
        if testrows(off):
            pruned = True
            offset += off
            height -= off
            new = set()
            for (x, y) in occupied:
                if y >= off:
                    new.add((x, y - off))
            occupied = new
            return True
    return False

def step():
    global jetidx, shapeidx, height, occupied, DOWN
    shape = [(x + 2, y + height + 3) for (x, y) in shapes[shapeidx]]
    shapeidx = (shapeidx + 1) % len(shapes)
    jetx = jets[jetidx]
    jetidx = (jetidx + 1) % len(jets)
    jetvec = (jetx, 0)
    if testshape(shape, jetvec):
        shape = [padd(p, jetvec) for p in shape]
    while testshape(shape, DOWN):
        shape = [padd(p, DOWN) for p in shape]
        jetx = jets[jetidx]
        jetidx = (jetidx + 1) % len(jets)
        jetvec = (jetx, 0)
        if testshape(shape, jetvec):
            shape = [padd(p, jetvec) for p in shape]
    occupied.update(shape)
    height = max(height, max([y+1 for (x, y) in shape]))

def printgrid():
    for y in range(height, -1, -1):
        print("".join(["#" if (x, y) in occupied else "." for x in range(7)]))
    print()

for n in range(1000000000000): # non-starter.
    step()
    prune()
    key = (shapeidx, jetidx, frozenset(occupied))
    prev = cache.get(key)
    if prev is None:
        cache[key] = (n, offset)
    else:
        (pn, poff) = prev
        dn = n - pn
        doff = offset - poff
        print("Found cycle of length", dn, ", height", doff)
        rem = 1000000000000 - 1 - n
        skip_cycles = rem // dn
        skipn = skip_cycles * dn
        skiph = skip_cycles * doff
        left = 1000000000000 - 1 - (n + skipn)
        offset += skiph
        for _ in range(left):
            step()
            prune()
        break
print(height + offset)
