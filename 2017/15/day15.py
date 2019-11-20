#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(i) for i in f.read().split(",")]

def part1():
    seen = 0
    aval = 783
    bval = 325
    for i in range(40000000):
        if i % 10000 == 0:
            print(i)
        aval = (aval * 16807) % 2147483647;
        bval = (bval * 48271) % 2147483647;
        if (aval & 0xFFFF) == (bval & 0xFFFF):
            seen += 1
    return seen

def run(filename):

    print(part1())
    #print(part2(hashes))

if __name__ == "__main__":
    run(sys.argv[1])
