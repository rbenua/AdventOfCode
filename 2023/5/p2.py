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
    for (dest, src, len) in map:
        if input < dest and (dest - input) < nextdiff:
            nextdiff = dest - input
        if input >= dest and input < dest + len:
            res = src + (input - dest)
            nextdiff = dest + len - input
            #print(f"{input} => {res}")
            return (res, nextdiff)
    #print(f"{input} => {input}")
    return (input, nextdiff)

def locseed(loc):
    init = loc 
    mindiff = max(seeds)
    for map in reversed(maps):
        (loc, diff) = followrev(loc, map)
        if diff < mindiff:
            mindiff = diff
    print(f"loc {init} => seed {loc}")
    return (loc, mindiff)

loc = 0
while True:
    (seed, diff) = locseed(loc)
    for (start, len) in seedpairs:
        if seed >= start and seed < start + len:
            print(loc)
            exit()
        if seed < start and seed + diff >= start:
            print(loc + start - seed)
            exit()
    loc += diff