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

seq = "".join(line.strip() for line in f).split(",")

def hash(s):
    curr = 0
    for c in s:
        curr += ord(c)
        curr = (curr * 17) % 256
    return curr

boxes = [[] for _ in range(256)]

for command in seq:
    if command[-1] == "-":
        label = command[:-1]
        box = boxes[hash(label)]
        for i in range(len(box)):
            if box[i][0] == label:
                del box[i]
                break
    else:
        [label, value] = command.split("=")
        value = int(value)
        box = boxes[hash(label)]
        found = False
        for (i, (l, v)) in enumerate(box):
            if l == label:
                box[i] = (label, value)
                found = True
        if not found:
            box.append((label, value))

total = 0
for (bn, box) in enumerate(boxes):
    for (ln, (label, value)) in enumerate(box):
        total += (bn + 1) * (ln + 1) * value
print(total)
        