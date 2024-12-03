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

modules = {}

class Broadcaster:
    def __init__(self, dests, name):
        self.dests = dests
        self.name = name
    def add_input(self, input):
        pass
    def pulse(self, high, src):
        return [(d, high, self.name) for d in self.dests]

class FlipFlop:
    state = False
    def __init__(self, dests, name):
        self.dests = dests
        self.name = name
    def add_input(self, input):
        pass
    def pulse(self, high, src):
        if not high:
            self.state = not self.state
            return [(d, self.state, self.name) for d in self.dests]
        return []

class Conj:
    def __init__(self, dests, name):
        self.inputs = {}
        self.dests = dests
        self.name = name
    def add_input(self, input):
        self.inputs[input] = False
    def pulse(self, high, src):
        self.inputs[src] = high
        out = not all(self.inputs.values())
        #print(f"conj {self.name} has inputs {self.inputs}, pulsing {out}")
        return [(d, out, self.name) for d in self.dests]

for line in f:
    m = re.match("([b%&])(\w+) -> ((?:\w+(?:, )?)+)", line).groups()
    op = m[0]
    name = m[1]
    dests = [d.strip() for d in m[2].split(",")]
    if op == 'b':
        modules["broadcaster"] = Broadcaster(dests, "broadcaster")
    elif op == '%':
        modules[name] = FlipFlop(dests, name)
    else:
        modules[name] = Conj(dests, name)

for m in modules.values():
    for d in m.dests:
        if d in modules:
            modules[d].add_input(m.name)

highs = 0
lows = 0
for _ in range(1000):
    to_process = deque([("broadcaster", False, None)])
    while len(to_process) > 0:
        (curr, high, src) = to_process.popleft()
        if high:
            highs += 1
            arrow = "-high->"
        else:
            lows += 1
            arrow = "-low->"
        #print(f"{src} {arrow} {curr}; {to_process}")
        if curr in modules:
            to_process.extend(modules[curr].pulse(high, src))
print(lows, highs, lows * highs)