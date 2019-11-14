#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [line.split() for line in f]

#note: i inverted this by accident, solved using `wc -l` - this number
def part1(lines):
    count = 0
    for line in lines:
        for s in line:
            if line.count(s) > 1:
                count += 1
                break
    return count

def part2(lines):
    count = 0
    for line in lines:
        s = set()
        for word in line:
            s.add(str(sorted(word)))
        print(s, line)
        if len(s) == len(line):
            count += 1
    return count


def run(filename):
    lines = read_input(filename)
    print(part1(lines))
    print(part2(lines))

if __name__ == "__main__":
    run(sys.argv[1])
