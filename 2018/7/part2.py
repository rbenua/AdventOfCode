#!/usr/bin/env python3

import sys
import re

pat = re.compile('Step (.) must be finished before step (.) can begin.')
edges = {}
ineligible = set() # steps that depend on any other steps

def tasktime(task):
    return int(task, 36) - 9 + 60

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

workertimes = [0,0,0,0,0]
workertasks = ["","","","",""]
elapsed = 0

while True: 
    # finish tasks, add their dependencies to the eligible list
    for i in range(5):
        if workertimes[i] <= 0 and workertasks[i] != "":
            todo = workertasks[i]
            workertasks[i] = ""
            for onward in edges[todo][1]:
                (c, l) = edges[onward]
                edges[onward] = (c - 1, l)
                if c == 1:
                    eligible.append(onward)
    
    eligible.sort()

    # pick new tasks off the beginning of the eligible list, reserve
    for i in range(5):
        if workertimes[i] <= 0 and len(eligible) != 0:
            todo = eligible.pop(0)
            workertasks[i] = todo
            workertimes[i] = tasktime(todo)
            
    # check if we're done (all workers idle)
    if max(workertimes) <= 0:
        print(elapsed)
        exit(0)

    # advance time to the next time something finishes
    to_add = min([i for i in workertimes if i > 0])
    elapsed += to_add
    workertimes = [i - to_add for i in workertimes]
