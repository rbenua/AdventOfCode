#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return f.read().strip().split(',')

def part1(inp):
    l = [0]
    cursor = 0
    for i in range(1, 2018):
        pos = ((cursor + inp) % len(l)) + 1
        l.insert(pos, i)
        cursor = pos
    return l[cursor + 1]



def part2(inp):
    l = 1
    cursor = 0
    current = 0
    for i in range(1, 50000001):
        pos = ((cursor + inp) % l) + 1
        if pos == 1:
            current = i
        l += 1
        cursor = pos
    return current

def run(filename):
    inp = int(filename)
    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    run(sys.argv[1])
