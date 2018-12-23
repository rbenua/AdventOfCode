#!/usr/bin/env python3

import sys, re
from z3 import *

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

def num_in_range(target):
    return len([p for (p, r) in pts if dist(p, target) <= r])

def zabs(x):
    return If(x >= 0, x, x * -1)

def zdist(x1, y1, z1, x2, y2, z2):
    return zabs(x1 - x2) + zabs(y1 - y2) + zabs(z1 - z2)

def build_solver():
    (x, y, z, npts, zdist) = Ints("x y z npts zdist")
    s = Optimize()
    s.add(npts == Sum([If(zdist(x,y,z, px, py, pz) <= r, 1, 0) for ((px, py, pz), r) in pts]))
    s.add(zdist == (zabs(x) + zabs(y) + zabs(z)))
    s.maximize(npts)
    s.minimize(zdist)
    return s


def run(filename):
    init(filename)
    s = build_solver()
    print(s.check())
    print(s.model())


if __name__ == "__main__":
    run(sys.argv[1])
