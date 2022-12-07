#!/usr/bin/env python3

import sys
import re
f = open(sys.argv[1], "r")

root = (None, {})
current = root
for line in f:
    if line[0] == "$":
        tokens = line.strip().split()
        if tokens[1] == "cd":
            if tokens[2] == "/":
                current = root
            elif tokens[2] == "..":
                current = current[0]
            else:
                current = current[1][tokens[2]]

    else:
        [size, name] = line.strip().split()
        if size == "dir":
            current[1][name] = (current, {})
        else:
            current[1][name] = int(size)

def calc(d):
    if type(d) is int:
        return (0, d)
    else:
        ans = size = 0
        for v in d[1].values():
            (a, s) = calc(v)
            ans += a
            size += s
        if size <= 100000:
            ans += size
        return (ans, size)

print(calc(root))