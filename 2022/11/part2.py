#!/usr/bin/env python3

import sys
import re
from math import lcm

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

class Monkey:
    activity = 0
monkeys = []

for segment in f.read().split("\n\n"):
    m = Monkey()
    lines = segment.split("\n")
    m.name = nums(lines[0])[0]
    m.items = nums(lines[1])
    m.op = eval("lambda old: " + lines[2].split("=")[1])
    m.test = nums(lines[3])[0]
    m.true = nums(lines[4])[0]
    m.false = nums(lines[5])[0]
    monkeys.append(m)

test_lcm = lcm(*[m.test for m in monkeys])

for round in range(10000):
    for m in monkeys:
        for i in m.items:
            new = m.op(i) % test_lcm 
            if new % m.test == 0:
                monkeys[m.true].items.append(new)
            else:
                monkeys[m.false].items.append(new)
        m.activity += len(m.items)
        m.items = []

acts = sorted([m.activity for m in monkeys])
print(acts[-2] * acts[-1])
