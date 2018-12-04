#!/usr/bin/env python3

import sys

twos = 0
threes = 0
with open(sys.argv[1], 'r') as f:
    for line in f.read().splitlines():
        counts = {}
        for c in line:
            counts[c] = counts.get(c, 0) + 1
        print(line)
        l = list(line)
        l.sort()
        print(''.join(l))
        #print(counts)
        v = counts.values()
        if 2 in v:
            print("two!")
            twos += 1
        if 3 in v:
            print("three!")
            threes += 1
        print('')

print(twos)
print(threes)
print(twos * threes)
