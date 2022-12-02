#!/usr/bin/env python3
import sys
f = open(sys.argv[1], "r")
score = 0
for line in f:
    theirs = ord(line[0]) - ord('A')
    yours = ord(line[2]) - ord('X')
    s = yours + 1
    if yours == theirs:
        s += 3
    elif yours == theirs + 1 or (yours == 0 and theirs == 2):
        s += 6
    score += s
    print(yours, theirs, s)
print(score)
