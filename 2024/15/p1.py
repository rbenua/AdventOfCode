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

move_dirs = {"^": up, ">": right, "v": down, "<": left}

walls = set()
boxes = set()

[grid, moves] = f.read().strip().split("\n\n")
x = 0
y = 0
for line in grid.split("\n"):
    x = 0
    for c in line.strip():
        if c == "#":
            walls.add((x, y))
        elif c == "O":
            boxes.add((x, y))
        elif c == "@":
            robot = (x, y)
        x += 1
    y += 1

mx = x
my = y

def push(box, dir):
    new = padd(box, dir)
    if new in walls:
        return False
    if new not in boxes or push(new, dir):
        boxes.remove(box)
        boxes.add(new)
        return True
    return False

for move in moves.strip():
    if move == "\n": 
        continue
    dir = move_dirs[move]
    new = padd(robot, dir)
    if new in walls:
        continue
    if new in boxes:
        if not push(new, dir):
            continue
    robot = new

print(sum(y * 100 + x for (x, y) in boxes))