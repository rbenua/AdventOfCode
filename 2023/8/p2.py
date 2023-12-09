#!/usr/bin/env python3

import sys
import re
import itertools
import math
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
starts = []

for line in f:
    m = reg.match(line)
    if m is not None:
        map[m.group(1)] = (m.group(2), m.group(3))
        if m.group(1)[2] == "A":
            starts.append(m.group(1))

lengths = []
offsets = []

for start in starts:
    length = 0
    curr = start
    seen = {(0, start): 0}
    end = -1
    print(f"checking start {start}")
    for (idx, dir) in itertools.cycle(enumerate(dirs)):
        if dir == "L":
            curr = map[curr][0]
        else:
            curr = map[curr][1]
        length += 1
        if curr[2] == "Z":
            end = length
        if (idx, curr) in seen:
            cyclen = length - seen[(idx, curr)]
            lengths.append(cyclen)
            offsets.append(end)
            break
        seen[(idx, curr)] = length

print(offsets)

mult = math.lcm(*lengths)
for (length, offset) in zip(lengths, offsets):
    print(f"length {length}, offset {offset}, {offset * (mult // length)}")
    pass


"""
findings:
each start in my input leads to a loop that does not contain the start but does reach exactly one endpoint
add the loop start offset to the offset of the end inside the loop to get the matching points mod loop size,
which is just the offset of the end the first time.
use CRT to solve.
"""