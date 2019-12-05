#!/usr/bin/env python3

import sys, re, collections

#pat = re.compile("(\w+) (\w)( [a-z]|[\d-]+)?")

def read_input(filename):
    with open(filename, 'r') as f:
        return tuple(map(int, f.read().split("-")))

def part1(inp):
    total = 0
    for i in range(inp[0], inp[1] + 1):
        s = str(i)
        incr = True
        pair = False
        for idx in range(5):
            if int(s[idx]) == int(s[idx + 1]):
                pair = True
            if int(s[idx]) > int(s[idx + 1]):
                incr = False
        if incr and pair:
            total += 1
    return total


def part2(inp):
    total = 0
    for i in range(inp[0], inp[1] + 1):
        s = str(i)
        incr = True
        pair = False
        for idx in range(5):
            if (int(s[idx]) == int(s[idx + 1])):
                #print("match", idx)
                valid = True
                if idx != 4 and int(s[idx + 2]) == int(s[idx + 1]):
                    #print("dq",idx + 2)
                    valid = False
                elif idx != 0 and int(s[idx - 1]) == int(s[idx]):
                    #print("dq",idx-1)
                    valid = False
                pair |= valid
            if int(s[idx]) > int(s[idx + 1]):
                incr = False
        if incr and pair:
            total += 1
    return total

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    run(sys.argv[1])
