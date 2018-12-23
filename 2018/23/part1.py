#!/usr/bin/env python3

import sys, re

pat = re.compile("pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)")

pts = []

def init(filename):
    global pts
    pts = []
    with open(filename, 'r') as f:
        for line in f:
            match = pat.search(line)
            if not match:
                raise ValueError
            nums = [int(d) for d in match.groups()]
            pts.append((tuple(nums[0:3]), nums[3]))

def dist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]) + abs(p1[2] - p2[2])

def run(filename):
    init(filename)
    (maxr, maxp) = max([(r, p) for (p, r) in pts])
    print(len([p for (p, r) in pts if dist(p, maxp) <= maxr]))

if __name__ == "__main__":
    run(sys.argv[1])
