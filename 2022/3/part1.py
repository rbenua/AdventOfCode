#!/usr/bin/env python3
import sys

def score(c):
    if c.islower():
        return ord(c) - ord('a') + 1
    else:
        return ord(c) - ord('A') + 27

f = open(sys.argv[1], "r")
total = 0
for line in f:
    all = line.strip()
    if all == "":
        continue
    first = {c for c in all[0:int(len(all)/2)]}
    second = {c for c in all[int(len(all)/2):]}
    inter = first.intersection(second).pop()
    total += score(inter)

print(total)
    
