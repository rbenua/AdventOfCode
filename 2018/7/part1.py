#!/usr/bin/env python3

import sys
import re

pat = re.compile('Step (.) must be finished before step (.) can begin.')
edges = {}
ineligible = set() # steps that depend on any other steps

with open(sys.argv[1], 'r') as f:
    for line in f:
        match = pat.match(line)
        if not match:
            print("could not match ", line)
            exit(1)
        (first, second) = match.groups()
        (ins, outs) = edges.get(first, (0, []))
        outs.append(second)
        edges[first] = (ins, outs)
        
        (ins, outs) = edges.get(second, (0, []))
        edges[second] = (ins + 1, outs)

        ineligible.add(second)

eligible = list(set(edges.keys()).difference(ineligible))
result = ""

while len(eligible) > 0:
    eligible.sort()
    todo = eligible[0]
    del eligible[0]
    result += todo
    for onward in edges[todo][1]:
        (c, l) = edges[onward]
        edges[onward] = (c - 1, l)
        if c == 1:
            eligible.append(onward)

print(result)
