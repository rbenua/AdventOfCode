#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cache
import time 

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
            walls.add((2*x, y))
            walls.add((2*x+1, y))
        elif c == "O":
            boxes.add((2*x, y))
        elif c == "@":
            robot = (2*x, y)
        x += 1
    y += 1

mx = x
my = y

def trypush(box, dir, doit):
    nl = padd(box, dir)
    nr = padd(nl, right)
    if nl in walls or nr in walls:
        return False
    res = True
    if dir == left:
        nb = padd(nl, left)
        if nb in boxes:
            res = trypush(nb, dir, doit)
    elif dir == right:
        if nr in boxes:
            res = trypush(nr, dir, doit)
    else:
        nbs = [padd(nl, left), nl, nr]
        res = all(trypush(b, dir, doit) for b in nbs if b in boxes)
    if res and doit:
        boxes.remove(box)
        boxes.add(nl)
    return res
    
def printgrid():
    print("\x1b[H")
    for y in range(my):
        x = 0
        while x < mx * 2 + 1:
            if (x, y) in walls:
                print("#", end="")
            elif (x, y) in boxes:
                print("[]", end="")
                x += 1
            elif (x, y) == robot:
                print("@", end="")
            else:
                print(" ", end="")
            x += 1
        print()

print("\x1b[2J")
print("\x1b[?25l")
for move in moves.strip():
    printgrid()
    if move == "\n": 
        continue
    dir = move_dirs[move]
    new = padd(robot, dir)
    if new in walls:
        continue
    b = None
    if new in boxes:
        b = new
    elif padd(new, left) in boxes:
        b = padd(new, left)
    if b is not None:
        if not trypush(b, dir, False):
            continue
        trypush(b, dir, True)
    robot = new

printgrid()
print("\x1b[?25h")
print(sum(y * 100 + x for (x, y) in boxes))