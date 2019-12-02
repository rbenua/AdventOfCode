#!/usr/bin/env python3

import sys, re, collections

#pat = re.compile("(\w+) (\w)( [a-z]|[\d-]+)?")

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(l) for l in f]

    
def part1(inp):
    total = 0
    for mass in inp:
        total += int(mass / 3) - 2
    return total

def part2(inp):
    total = 0
    for mass in inp:
        t = int(mass / 3) - 2
        while t > 0:
            total += t
            t = int(t / 3) - 2
    return total
        


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    run(sys.argv[1])
