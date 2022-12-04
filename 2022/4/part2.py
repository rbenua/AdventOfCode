#!/usr/bin/env python3
import sys

f = open(sys.argv[1], "r")

total = 0
for line in f:
    [a1, b1, a2, b2] = [int(x) for x in line.replace("-", ",").split(",")]
    if (a1 <= a2 <= b1) or (a1 <= b2 <= b1) or (a2 <= a1 <= b2) or (a2 <= b1 <= b2):
        total += 1
print(total)
