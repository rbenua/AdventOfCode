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

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

fwd = {}
rev = defaultdict(lambda: [])
r = re.compile("(\w{4}) (.) (\w{4})")
for line in f:
    [dst, rest] = line.strip().split(": ")
    m = r.match(rest)
    if m is None:
        fwd[dst] = int(rest)
    else:
        fwd[dst] = m.groups()
        rev[m.group(1)].append(dst)
        rev[m.group(3)].append(dst)

def evaluate(node):
    match fwd[node]:
        case [fst, op, snd]:
            match op:
                case "+":
                    return evaluate(fst) + evaluate(snd)
                case "-":
                    return evaluate(fst) - evaluate(snd)
                case "*":
                    return evaluate(fst) * evaluate(snd)
                case "/":
                    return evaluate(fst) // evaluate(snd)
                case _:
                    print("Bad operator", op)
                    raise ValueError
        case x:
            return x

print(evaluate("root"))
