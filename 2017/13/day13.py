#!/usr/bin/env python3

import sys, re, collections


def read_input(filename):
    with open(filename, 'r') as f:
        d = []
        for line in f:
            l = line.split(":")
            d.append((int(l[0]), int(l[1])))
        return d


def part1(grid):
    total = 0
    for (depth, rg) in grid:
        if depth % ((rg - 1) * 2) is 0:
            total += depth * rg
    return total

def caught(grid, delay):
    for (depth, rg) in grid:
        if (depth + delay) % ((rg - 1) * 2) is 0:
            return True
    return False

def part2(grid):
    delay = 0
    while caught(grid, delay):
        delay += 1
    return delay

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
