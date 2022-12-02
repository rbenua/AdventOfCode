#!/usr/bin/env python3
f = open("input.txt", "r")
cur = 0
all = []
for line in f:
    if len(line) > 1:
        cur += int(line.strip())
    else:
        all.append(cur)
        cur = 0

all.sort()
print(sum(all[-3:]))
