#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(line) for line in f]

def part1(lines):
    pc = 0
    steps = 0
    while pc >= 0 and pc < len(lines):
        newpc = pc + lines[pc]
        lines[pc] += 1
        pc = newpc
        steps += 1
    return steps

def part2(lines):
    pc = 0
    steps = 0
    while pc >= 0 and pc < len(lines):
        newpc = pc + lines[pc]
        if lines[pc] >= 3:
            lines[pc] -= 1
        else:
            lines[pc] += 1
        pc = newpc
        steps += 1
    return steps


def run(filename):
    lines = read_input(filename)
    print(part1(lines))
    lines = read_input(filename)
    print(part2(lines))

if __name__ == "__main__":
    run(sys.argv[1])
