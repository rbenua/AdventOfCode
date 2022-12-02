#!/usr/bin/env python3
import sys
f = open(sys.argv[1], "r")
score = 0
for line in f:
    theirs = ord(line[0]) - ord('A')
    result = ord(line[2]) - ord('Y')
    yours = (result + theirs) % 3;
    s = yours + 1
    if yours == theirs:
        s += 3
    elif yours == (theirs + 1) % 3:
        s += 6
    score += s
print(score)
