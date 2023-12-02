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

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

lines = [l.rstrip() for l in f]
insns = deque(lines.pop())
lines.pop()


DIRS = [(1,0),(0,1),(-1,0),(0,-1)]

walls = set()
grid = set()

for (y, line) in enumerate(lines):
    w = set((x, y) for (x, c) in enumerate(line) if c == "#")
    walls |= w
    g = set((x, y) for (x, c) in enumerate(line) if c == '.')
    grid |= w
    grid |= g

facing = 0
pos = (0,0)
trail = {}
while (pos not in grid) or (pos in walls):
    pos = padd(pos, DIRS[facing])

def boundary(np):
    (nx, ny) = np
    nf = -1
    if ny == -1 and (50 <= nx < 100):
        np = (0, 150 + nx - 50)
        nf = 0
    elif ny == -1 and (100 <= nx < 150):
        np = (nx - 100, 199)
        nf = 3
    elif nx == 150 and (0 <= ny < 50):
        np = (99, 149 - ny)
        nf = 2
    elif ny == 50 and 100 <= nx < 150:
        np = (99, 50 + (nx - 100))
        nf = 2
    elif nx == 100 and 50 <= ny < 100:
        np = (100 + (ny - 50), 49)
        nf = 3
    elif nx == 100 and 100 <= ny < 150:
        np = (149, 49 - (ny - 100))
        nf = 2
    elif ny == 150 and 50 <= nx < 100:
        np = (49, 150 + (nx - 50))
        nf = 2
    elif nx == 50 and 150 <= ny < 200:
        np = (50 + (ny - 150), 149)
        nf = 3
    elif ny == 200 and 0 <= nx < 50:
        np = (nx + 100, 0)
        nf = 1
    elif nx == -1 and 150 <= ny < 200:
        np = ((50 + ny - 150), 0)
        nf = 1
    elif nx == -1 and (100 <= ny < 150):
        np = (50, (149 - ny))
        nf = 0
    elif ny == 99 and 0 <= nx < 50:
        np = (50, 50 + nx)
        nf = 0
    elif nx == 49 and 50 <= ny < 100:
        np = (ny - 50, 100)
        nf = 1
    elif nx == 49 and 0 <= ny < 50:
        np = (0, 100 + (49 - ny))
        nf = 0
    else:
        print("Unexpected boundary condition", np, facing)
        exit(-1)
    return np, nf

def forward(pos, facing, amt):
    #print("forward", pos, facing, amt, "= ", end="")
    for _ in range(amt):
        trail[pos] = facing
        np = padd(pos, DIRS[facing])
        nf = facing
        if np not in grid:
            over, nf = boundary(np)
            print("Moving from", np, facing, "to", over, nf)
            rev = pmul(DIRS[nf], -1)
            ckpos, ckf = boundary(padd(over, rev))
            if ckpos != pos or ckf != (facing + 2) % 4:
                print("Check failed!")
                print("initial:", pos, facing)
                print("New pos:", over, nf)
                print("Return: ", ckpos, ckf)
                exit(-1)
            np = over
        if np in walls:
            break
        pos = np
        facing = nf
    #print(pos)
    trail[pos] = facing
    return pos

amt_acc = ""
while len(insns) > 0:
    c = insns.popleft()
    if c.isnumeric():
        amt_acc += c
    elif c == "R":
        if amt_acc != "":
            amt = int(amt_acc)
            pos = forward(pos, facing, amt)
            amt_acc = ""
        facing = (facing + 1) % 4
        print(amt, "R")
        printgrid()

    elif c == "L":
        if amt_acc != "":
            amt = int(amt_acc)
            pos = forward(pos, facing, amt)
            amt_acc = ""
        facing = (facing - 1) % 4

if amt_acc != "":
    amt = int(amt_acc)
    pos = forward(pos, facing, amt)
    amt_acc = ""

def ch(x, y):
    if (x, y) in trail:
        return ">v<^"[trail[(x,y)]]
    elif (x, y) in walls:
        return '#'
    elif (x, y) in grid:
        return '.'
    else:
        return ' '


def printgrid():
    for y in range(200):
        print("".join(ch(x, y) for x in range(150)))
        
printgrid()

print(pos, facing)
print(1000 * (pos[1] + 1) + 4 * (pos[0] + 1) + facing)
