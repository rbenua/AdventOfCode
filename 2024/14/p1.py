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

mx = 101
my = 103
if len(sys.argv) > 2:
    mx = int(sys.argv[2])
    my = int(sys.argv[3])

robots = []
for line in f:
    [x, y, dx, dy] = nums(line)
    robots.append(((x, y), (dx, dy)))

for iter in range(100):
    for i in range(len(robots)):
        (p, v) = robots[i]
        np = pmod(padd(p, v), mx, my)
        robots[i] = (np, v)

quads = [0, 0, 0, 0]

for ((x, y), _) in robots:
    qi = 0
    if x > (mx // 2):
        qi += 1
    elif x == (mx // 2):
        continue 
    if y > (my // 2):
        qi += 2
    elif y == (my // 2):
        continue
    quads[qi] += 1

print(quads[0] * quads[1] * quads[2] * quads[3])