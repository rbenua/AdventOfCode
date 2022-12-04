#!/usr/bin/env python3
import sys

f = open(sys.argv[1], "r")

total = 0
for line in f:
    [a1, b1, a2, b2] = [int(x) for x in line.replace("-", ",").split(",")]
    if (a1 >= a2 and b1 <= b2) or (a2 >= a1 and b2 <= b1):
        total += 1
print(total)
