#!/usr/bin/env python3
f = open("input.txt", "r")
cur = 0
max = 0
for line in f:
    if len(line) > 1:
        cur += int(line.strip())
    else:
        if cur > max:
            max = cur
        cur = 0

print(max)
