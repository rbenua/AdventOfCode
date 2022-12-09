#!/usr/bin/env python3

import sys
import re
f = open(sys.argv[1], "r")

moves = []
for line in f:
    [m, a] = line.strip().split()
    moves.append((m, int(a)))

(hx, hy) = (0, 0)
(tx, ty) = (0, 0)
visited = {(0, 0)}
dirs = {"U":(0,1), "D":(0,-1), "R":(1,0), "L":(-1,0)}

def clamp(i, l, u):
    return max(l, min(u, i))

def follow(hx, hy, tx, ty):
    if (hx - 1) <= tx <= (hx + 1) and (hy - 1) <= ty <= (hy + 1):
        return (tx, ty)
    else:
        dx = clamp(hx - tx, -1, 1)
        dy = clamp(hy - ty, -1, 1)
        return (tx + dx, ty + dy)

for (dir, amt) in moves:
    (hdx, hdy) = dirs[dir]
    for _ in range(amt):
        hx += hdx
        hy += hdy
        (tx, ty) = follow(hx, hy, tx, ty)
        visited.add((tx, ty))
print(len(visited))
