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

rates = {}
paths = {}
r = re.compile("Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)$")
for line in f:
    m = r.search(line.strip())
    name = m.group(1)
    rate = int(m.group(2))
    outs = m.group(3).split(", ")
    rates[name] = rate
    paths[name] = outs

nzrates = {k:v for (k, v) in rates.items() if v != 0 or k == "AA"}

def bfs(src):
    dsts = []
    to_visit = deque([(0, src)])
    visited = {src}
    while len(to_visit) > 0:
        (cdist, curr) = to_visit.popleft()
        if curr in nzrates and curr != src:
            dsts.append((curr, cdist))
            if len(dsts) == len(nzrates) - 1:
                return dsts
        for nbr in paths[curr]:
            if nbr not in visited:
                visited.add(nbr)
                to_visit.append((cdist + 1, nbr))
    return dsts

nzpaths = {src:bfs(src) for src in nzrates.keys()}

def maxscore(t, sofar, mpos, mdone, epos, edone, visited):
    best = sofar
    if mdone == t:
        for (next, dist) in nzpaths[mpos]:
            nmdone = mdone + dist + 1
            if nmdone <= 26 and next not in visited:
                score = sofar + nzrates[next] * (26 - nmdone)
                nbest = maxscore(min(nmdone, edone), score, next, nmdone, epos, edone, visited | {next})
                if nbest > best:
                    best = nbest
    else:
        for (next, dist) in nzpaths[epos]:
            nedone = edone + dist + 1
            if nedone <= 26 and next not in visited:
                score = sofar + nzrates[next] * (26 - nedone)
                nbest = maxscore(min(mdone, nedone), score, mpos, mdone, next, nedone, visited | {next})
                if nbest > best:
                    best = nbest
    return best

print(maxscore(0, 0, "AA", 0, "AA", 0, {"AA"}))
