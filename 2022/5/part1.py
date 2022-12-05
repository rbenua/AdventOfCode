#!/usr/bin/env python3

import sys
import re
f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

stacks = []
for line in f:
    if line[1] == '1':
        f.readline()
        break
    for (i, idx) in zip(range(1, len(line)), range(1, len(line), 4)):
        if line[idx].isalpha():
            if i >= len(stacks):
                stacks.extend([[] for _ in range(i + 1 - len(stacks))])
            stacks[i].insert(0, line[idx])

for line in f:
    [count, src, dst] = nums(line)
    for _ in range(count):
        stacks[dst].append(stacks[src].pop())

print(''.join([l.pop() for l in stacks[1:]]))
