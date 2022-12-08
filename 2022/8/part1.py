#!/usr/bin/env python3

import sys
import re
f = open(sys.argv[1], "r")

hts = []
for line in f:
    hts.append([int(c) for c in line.strip()])

visible = set()
for (i, row) in enumerate(hts):
    max = -1
    for (j, tree) in enumerate(row):
        if tree > max:
            visible.add((i, j))
            max = tree
            if max == 9:
                break
    max = -1
    for (j, tree) in reversed(list(enumerate(row))):
        if tree > max:
            visible.add((i, j))
            max = tree
            if max == 9:
                break

for j in range(len(hts[0])):
    max = -1
    for i in range(len(hts)):
        tree = hts[i][j]
        if tree > max:
            visible.add((i, j))
            max = tree
            if max == 9:
                break
    max = -1
    for i in reversed(range(len(hts))):
        tree = hts[i][j]
        if tree > max:
            visible.add((i, j))
            max = tree
            if max == 9:
                break

print(len(visible))
