#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from functools import cache

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

def bounds_check(p, mx, my):
    return p[0] >= 0 and p[0] < mx and p[1] >= 0 and p[1] < my

[inits, gates] = f.read().split("\n\n")

values = {}
for line in inits.split("\n"):
    [name, value] = line.strip().split(": ")
    values[name] = int(value)

ops = {}
for line in gates.strip().split("\n"):
    [ins, name] = line.strip().split(" -> ")
    ops[name] = ins.split(" ")

names = set(values.keys()).union(ops.keys())

def calculate(name):
    if name in values:
        return values[name]
    [lhs, op, rhs] = ops[name]
    if op == "AND":
        res = calculate(lhs) & calculate(rhs)
    elif op == "OR":
        res = calculate(lhs) | calculate(rhs)
    else:
        res = calculate(lhs) ^ calculate(rhs)
    values[name] = res
    return res

def expand(name):
    if name in values:
        return name
    [lhs, op, rhs] = ops[name]
    return [expand(lhs), op, expand(rhs), name]

@cache
def carry(bits):
    if bits == 0:
        return ["x00", "AND", "y00"]
    else:
        return [[[f"x{bits:02}", "XOR", f"y{bits:02}"], "AND", carry(bits - 1)], 
                "OR", 
                [f"x{bits:02}", "AND", f"y{bits:02}"]]

def adder(bits):
    if bits == 0:
        return ["x00", "XOR", "y00"]
    else:
        return [[f"x{bits:02}", "XOR", f"y{bits:02}"], "XOR", carry(bits - 1)]

def compare(e1, e2):
    if isinstance(e1, str):
        if isinstance(e2, str):
            return e1 == e2
        else:
            return False
    elif isinstance(e2, str):
        return False
    [l1, o1, r1, name] = e1
    [l2, o2, r2] = e2
    return o1 == o2 and ((compare(l1, l2) and compare(r1, r2)) or (compare(l1, r2) and compare(r1, l2)))

def find(e2):
    for node in names:
        if compare(expand(node), e2):
            return node
    return None

def swap(name1, name2):
    temp = ops[name1]
    ops[name1] = ops[name2]
    ops[name2] = temp

def fix(e1, e2):
    assert(isinstance(e1, list) and isinstance(e2, list))
    [l1, o1, r1, name1] = e1
    [l2, o2, r2] = e2
    if o1 != o2:
        name2 = find(e2)
        assert(name2 != None)
        swap(name1, name2)
        return (name1, name2)
    res = None
    if compare(l1, l2):
        return fix(r1, r2)
    elif compare(r1, r2):
        return fix(l1, l2)
    elif compare(l1, r2):
        return fix(r1, l2)
    elif compare(r1, l2):
        return fix(l1, r2)
    else:
        name2 = find(e2)
        assert(name2 != None)
        swap(name1, name2)
        return (name1, name2)


swaps = set()
bit = 0
while f"z{bit:02}" in names and f"x{bit:02}" in values:
    a = adder(bit)
    e = expand(f"z{bit:02}")
    if not compare(e, a):
        print(f"error at bit {bit}")
        up = fix(e, a)
        print(f"fixed by swapping {up}")
        swaps.update(up)
    bit += 1

print(",".join(sorted(swaps)))