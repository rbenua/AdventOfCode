#!/usr/bin/env python3

import sys
import re
from collections import deque
from functools import cmp_to_key

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

def nbrs(p, mx, my):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if 0 <= x < mx and 0 <= y < my]

lists = [[[2]], [[6]]]
for line in f:
    if line != "\n":
        lists.append(eval(line.strip()))

def compare(left, right):
    if type(left) is int and type(right) is int:
        if left < right:
            return -1
        elif left > right:
            return 1
        else:
            return 0
    elif type(left) is int:
        return compare([left], right)
    elif type(right) is int:
        return compare(left, [right])
    else: # both lists
        for (a, b) in zip(left, right):
            r = compare(a, b)
            if r != 0:
                return r
        if len(left) < len(right):
            return -1
        elif len(left) > len(right):
            return 1
        return 0

lists.sort(key=cmp_to_key(compare))
print((lists.index([[2]]) + 1) * (lists.index([[6]]) + 1))
