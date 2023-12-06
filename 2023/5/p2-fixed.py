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

seeds = nums(f.readline())
seedpairs = list(zip(seeds[::2], seeds[1::2]))

f.readline()
f.readline()

maps = []

line = f.readline()
curr_map = []
while line != "":
    if(line.strip() == ""):
        f.readline()
        line = f.readline()
        maps.append(curr_map)
        curr_map = []
        continue
    curr_map.append(tuple(nums(line)))
    line = f.readline()
maps.append(curr_map)

def follow(input, map):
    for (dest, src, len) in map:
        if input >= src and input < src + len:
            res = dest + (input - src)
            #print(f"{input} => {res}")
            return res
    #print(f"{input} => {input}")
    return input

def followrev(input, map):
    nextdiff = max(seeds)
    outdiff = max(seeds)
    rs = []
    id = True
    for (dest, src, len) in map:
        if input >= src and input < src + len:
            id = False
        if input < dest and (dest - input) < outdiff:
            outdiff = dest - input
        if input >= dest and input < dest + len:
            res = src + (input - dest)
            nextdiff = dest + len - input
            rs.append(res)
    if id:
        rs.append(input)
        if outdiff < nextdiff:
            nextdiff = outdiff
    #print(f"{input} => {rs}, diff {nextdiff}")
    return (rs, nextdiff)

def locseeds(loc):
    curr = [loc] 
    mindiff = max(seeds)
    for map in reversed(maps):
        next = []
        for c in curr:
            (res, diff) = followrev(c, map)
            if diff < mindiff:
                mindiff = diff
            next += res
        curr = next
    print(f"loc {loc} => seed {curr}, mindiff {mindiff}")
    return (curr, mindiff)

loc = 0
while True:
    (seeds, diff) = locseeds(loc)
    for (start, len) in seedpairs:
        for seed in seeds:
            if seed >= start and seed < start + len:
                print(loc)
                exit()
            if seed < start and seed + diff >= start:
                print(loc + start - seed)
                exit()
    loc += diff