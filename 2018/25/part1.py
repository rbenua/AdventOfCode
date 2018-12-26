#!/usr/bin/env python3

import sys

consts = []

def dist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]) + abs(p1[2] - p2[2]) + abs(p1[3] - p2[3])

def init(filename):
    global consts
    consts = []
    with open(filename, 'r') as f:
        for line in f:
            pt = tuple([int(d) for d in line.split(',')])
            found = False
            first = None
            for const in consts:
                if any([dist(pt, pc) <= 3 for pc in const]):
                    if not first:
                        const.append(pt)
                        found = True
                        first = const
                    else:
                        first += const
                        consts.remove(const)
            if not found:
                consts.append([pt])

if __name__ == "__main__":
    init(sys.argv[1])
    print(len(consts))
