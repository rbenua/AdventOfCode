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

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

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

arrows = {"^": up, ">": right, "v": down, "<": left}

grid = [list(line.strip()) for line in f]

maxy = len(grid)
maxx = len(grid[0])
start = (grid[0].index('.'), 0)
end = (grid[-1].index('.'), maxy - 1)

nodes = set()
for y in range(maxy):
    for x in range(maxx):
        if grid[y][x] != ".":
            continue
        nn = 0
        for n in nbrs((x, y), maxx, maxy):
            if grid[n[1]][n[0]] != '#':
                nn += 1
        if nn != 2:
            nodes.add((x, y))
print(nodes)

def in_legal(new, dir):
    (x, y) = new
    if grid[y][x] == '.':
        return True
    elif grid[y][x] == '#':
        return False
    else:
        return arrows[grid[y][x]] != dir

def in_edges(node):
    seen = set()
    to_search = deque([(node, 0)])
    res = []
    while len(to_search) > 0:
        (curr, cdist) = to_search.popleft()
        if curr in nodes and curr != node:
            res.append((cdist, curr))
            continue
        seen.add(curr)
        for new in nbrs(curr, maxx, maxy):
            dir = padd(new, pmul(curr, -1))
            if new not in seen and in_legal(new, dir):
                to_search.append((new, cdist + 1))
    return res

es = {n: in_edges(n) for n in nodes}

memo = {}
def search(goal, used):
    if goal == start:
        return 0
    if (goal, used) in memo:
        return memo[(goal, used)]
    u = frozenset(used.union({goal}))
    res = max(search(new, u) + d for (d, new) in es[goal])
    memo[(goal, used)] = res
    return res

print(search(end, frozenset()))