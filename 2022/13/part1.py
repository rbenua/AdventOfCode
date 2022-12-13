#!/usr/bin/env python3

import sys
import re
from collections import deque

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

def nbrs(p, mx, my):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if 0 <= x < mx and 0 <= y < my]

pairs = []
for idx, seg in enumerate(f.read().split("\n\n")):
    (first, second) = seg.strip().split("\n")
    pairs.append((idx + 1, eval(first), eval(second)))

def compare(left, right):
    if type(left) is int and type(right) is int:
        if left < right:
            return True
        elif left > right:
            return False
        else:
            return None
    elif type(left) is int:
        return compare([left], right)
    elif type(right) is int:
        return compare(left, [right])
    else: # both lists
        for (a, b) in zip(left, right):
            r = compare(a, b)
            if r is not None:
                return r
        if len(left) < len(right):
            return True
        elif len(left) > len(right):
            return False
        return None

total = 0
for (idx, a, b) in pairs:
    if compare(a, b) == True:
        print(idx)
        total += idx
print(total)
