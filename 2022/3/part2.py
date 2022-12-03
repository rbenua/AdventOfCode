#!/usr/bin/env python3
import sys

def score(c):
    if c.islower():
        return ord(c) - ord('a') + 1
    else:
        return ord(c) - ord('A') + 27

f = open(sys.argv[1], "r")
total = 0
lines = [line for line in f]
while len(lines) > 0:
    all1 = {c for c in lines.pop().strip()}
    all2 = {c for c in lines.pop().strip()}
    all3 = {c for c in lines.pop().strip()}
    inter = all1.intersection(all2).intersection(all3).pop()
    total += score(inter)

print(total)
    
