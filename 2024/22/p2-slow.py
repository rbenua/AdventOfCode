#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cache

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
dirs = [up, right, down, left]

def turn(dir, right):
    delta = 1 if right else -1
    return dirs[(dirs.index(dir) + delta) % 4]

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def psub(a, b):
    return (a[0] - b[0], a[1] - b[1])

def pmul(a, n):
    return (a[0] * n, a[1] * n)

def pmod(p, mx, my):
    return (p[0] % mx, p[1] % my)

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

def clamp(p, mx, my):
    return (min(mx - 1, max(p[0], 0)), min(my - 1, max(p[1], 0)))

def bounds_check(p, mx, my):
    return p[0] >= 0 and p[0] < mx and p[1] >= 0 and p[1] < my

def step(seed):
    seed = ((seed * 64) ^ seed) % 16777216
    seed = ((seed // 32) ^ seed) % 16777216
    seed = ((seed * 2048) ^ seed) % 16777216
    return seed

sequences = []
for seed in f.readlines():
    if seed.strip() == "":
        continue
    seed = int(seed.strip())
    values = []
    deltas = []
    prev = seed
    for rep in range(2000):
        seed = step(seed)
        delta = (seed % 10) - (prev % 10)
        values.append(seed % 10)
        deltas.append(delta)
        prev = seed
    sequences.append((values, deltas))
    #print(f"{orig}: {seed}")

def search(list, pattern):
    for i in range(len(list) - len(pattern)):
        if list[i:i+len(pattern)] == pattern:
            return i 
    return None

def price(sequence, pattern):
    (values, deltas) = sequence
    i = search(deltas, pattern)
    if i is not None:
        return values[i + len(pattern) - 1]
    return 0

def pat_value(pattern):
    return sum(price(seq, pattern) for seq in sequences)

print(max(pat_value([d0, d1, d2, d3]) for d0 in range(-9, 10) for d1 in range(-9, 10) for d2 in range(-9, 10) for d3 in range(-9, 10)))