#!/usr/bin/env python3

import sys

with open(sys.argv[1], 'r') as f:
    lines = f.read().splitlines()
    for startidx in range(len(lines) - 1):
        first = lines[startidx]
        rest = lines[startidx + 1:]
        for other in rest:
            #print("comparing {}, {}".format(first, other))
            diffs = 0
            for (f, o) in zip(list(first), list(other)):
                if f != o:
                    diffs += 1
            if diffs == 1:
                print("found {}, {}".format(first, other))
                exit(0)
