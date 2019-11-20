#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return f.read().strip().split(",")


def part1(path):
    ux = 0
    uy = 0
    dx = 0
    dy = 0
    for d in path:
        if d == 'n':
            uy += 1
            dy += 1
        elif d == 's':
            uy -= 1
            dy -= 1
        elif d == 'ne':
            ux += 1
            dx += 1
            dy += 1
        elif d == 'se':
            ux += 1
            uy -= 1
            dx += 1
        elif d == 'nw':
            ux -= 1
            uy += 1
            dx -= 1
        elif d == 'sw':
            ux -= 1
            dx -= 1
            dy -= 1
        else:
            print("found bad direction", d)
    return min(abs(dx) + abs(dy), abs(ux) + abs(uy))

def part2(path):
    ux = 0
    uy = 0
    dx = 0
    dy = 0
    mdist = 0
    for d in path:
        if d == 'n':
            uy += 1
            dy += 1
        elif d == 's':
            uy -= 1
            dy -= 1
        elif d == 'ne':
            ux += 1
            dx += 1
            dy += 1
        elif d == 'se':
            ux += 1
            uy -= 1
            dx += 1
        elif d == 'nw':
            ux -= 1
            uy += 1
            dx -= 1
        elif d == 'sw':
            ux -= 1
            dx -= 1
            dy -= 1
        else:
            print("found bad direction", d)
        mdist = max(mdist, min(abs(dx) + abs(dy), abs(ux) + abs(uy)))
    return mdist


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
