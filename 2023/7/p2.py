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

card_strengths = ' J23456789TQKA'

def type_strength(hand):
    hset = defaultdict(lambda: 0)
    for c in hand:
        hset[c] += 1
    jokers = hset["J"]
    hset["J"] = 0
    counts = sorted(hset.values(), reverse=True)
    counts[0] += jokers
    if counts[0] == 5:
        return 6
    elif counts[0] == 4:
        return 5
    elif counts[0] == 3 and counts[1] == 2:
        return 4
    elif counts[0] == 3:
        return 3
    elif counts[0] == 2 and counts[1] == 2:
        return 2
    elif counts[0] == 2:
        return 1
    else:
        return 0

def handstrength(p):
    hand = p[0]
    s = type_strength(hand)
    for c in hand:
        s *= len(card_strengths)
        s += card_strengths.find(c)
    return s

plays = []
for line in f:
    [hand, bid] = line.strip().split()
    plays.append((hand, int(bid)))

total = 0
for (x, (hand, bid)) in enumerate(sorted(plays, key=handstrength)):
    total += (x + 1) * bid
print(total)