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

codes = list(f.readlines())

kpd = {"7": (0, 0), "8": (1, 0), "9": (2, 0),
       "4": (0, 1), "5": (1, 1), "6": (2, 1),
       "1": (0, 2), "2": (1, 2), "3": (2, 2),
                    "0": (1, 3), "A": (2, 3)}

arrow = {             "^": (1, 0), "A": (2, 0),
         "<": (0, 1), "v": (1, 1), ">": (2, 1)}

def push_seqs(pad, curr_pos, dest):
    xs = list(range(min(curr_pos[0], dest[0]), max(curr_pos[0], dest[0]) + 1))
    ys = list(range(min(curr_pos[1], dest[1]), max(curr_pos[1], dest[1]) + 1))
    x_valid = all((x, curr_pos[1]) in pad.values() for x in xs)
    y_valid = all((curr_pos[0], y) in pad.values() for y in ys)
    lr = ("<" if dest[0] < curr_pos[0] else ">") * abs(dest[0] - curr_pos[0])
    ud = ("^" if dest[1] < curr_pos[1] else "v") * abs(dest[1] - curr_pos[1])
    #print(f"xs={xs}, ys={ys}, x_valid={x_valid}, y_valid={y_valid}, lr={lr}, ud={ud}")
    res = []
    if x_valid:
        res.append(lr + ud + "A")
    if y_valid:
        res.append(ud + lr + "A")
    #print(f"push_seqs({"keypad" if pad == kpd else "arrows"}, {curr_pos}, {dest}) = {res}")
    return res

@cache
def arrow_sequence(keypad, sequence, depth):
    if keypad:
        pad = kpd
    else:
        pad = arrow
    if depth == 0:
        return len(sequence)
    pos = pad["A"]
    res = 0
    for c in sequence:
        if c == " ":
            continue
        dest = pad[c]
        res += min(arrow_sequence(False, p, depth - 1) for p in push_seqs(pad, pos, dest))
        pos = dest
    #print(f"{sequence} -> {res}")
    return res

total = 0
for code in codes:
    code = code.strip()
    if code == "":
        continue
    last_seq = arrow_sequence(True, code, 26)
    print(f"{code}: {last_seq}")
    total += last_seq * int(code[:-1])

print(total)