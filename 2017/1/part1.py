#!/usr/bin/env python3

import sys, re

pat = re.compile("([a-z]+),? (\d+),? (\d+),? (\d+)")


def read_input(filename):
    with open(filename, 'r') as f:
        return [int(i) for i in f.read().strip()]

def result(l):
    total = 0
    for idx in range(len(l)):
        if l[idx] == l[idx-1]:
            total += l[idx]
    return total

def run(filename):
    l = read_input(filename)
    print(result(l))

if __name__ == "__main__":
    run(sys.argv[1])
