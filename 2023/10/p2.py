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

adj = {}
start = (0, 0)

max_y = 0
max_x = 0
input = [line.strip() for line in f]

for (y, line) in enumerate(input):
    max_y = y
    for (x, c) in enumerate(line):
        max_x = x
        if c == 'S':
            start = (x, y)
        elif c == '|':
            adj[(x, y)] = [(x, y+1), (x, y-1)]
        elif c == '-':
            adj[(x, y)] = [(x-1, y), (x+1, y)]
        elif c == 'L':
            adj[(x, y)] = [(x, y-1), (x+1, y)]
        elif c == 'J':
            adj[(x, y)] = [(x, y-1), (x-1, y)]
        elif c == '7':
            adj[(x, y)] = [(x, y+1), (x-1, y)]
        elif c == 'F':
            adj[(x, y)] = [(x, y+1), (x+1, y)]

adj[start] = []
for (k, v) in adj.items():
    if start in v:
        adj[start].append(k)

seen = set()
to_process = deque([(0, start)])
curr_dist = 0
while len(to_process) > 0:
    (curr_dist, curr) = to_process.popleft()
    seen.add(curr)
    for new in adj[curr]:
        if new not in seen:
            to_process.append((curr_dist + 1, new))

total = 0
ins = set()
for curr_y in range(max_y+1):
    for curr_x in range(max_x+1):
        if (curr_x, curr_y) not in seen:
            lefts = [x for x in range(curr_x) if (x, curr_y) in seen]
            rights = [x for x in range(curr_x, max_x) if (x, curr_y) in seen]
            passes_left = 0
            last = ""
            for x in lefts:
                if input[curr_y][x] == '|':
                    passes_left += 1
                elif input[curr_y][x] == 'J':
                    if last == 'F':
                        passes_left += 1
                elif input[curr_y][x] == '7':
                    if last == 'L':
                        passes_left += 1
                if input[curr_y][x] in 'LJF7':
                    last = input[curr_y][x]
            if passes_left % 2 == 1:
                total += 1
                ins.add((curr_x, curr_y))
            else:
                #print(f"{curr_x}, {curr_y} out")
                pass

for y in range(max_y+1):
    for x in range(max_x+1):
        if (x, y) in seen:
            print(input[y][x], end="")
        elif (x,y) in ins:
            print("I", end="")
        else:
            print("O", end="")
    print()
print(total)