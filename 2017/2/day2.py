#!/usr/bin/env python3

import sys, re


def read_input(filename):
    with open(filename, 'r') as f:
        return [[int(i) for i in line.split()] for line in f]

def part1(sheet):
    total = 0
    for line in sheet:
        total += max(line) - min(line)
    return total

def find_div(line):
    for i in range(len(line)):
        a = line[i]
        for b in line[i+1:]:
            if (max(a, b) % min(a, b)) is 0:
                return int(max(a, b) / min(a, b))

def part2(sheet):
    total = 0
    for line in sheet:
        total += find_div(line)
    return total


def run(filename):
    l = read_input(filename)
    print(part1(l))
    print(part2(l))

if __name__ == "__main__":
    run(sys.argv[1])
