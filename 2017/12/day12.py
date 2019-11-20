#!/usr/bin/env python3

import sys, re, collections

pat = re.compile("(\d+) <-> ([\d, ]+)\n")

def read_input(filename):
    with open(filename, 'r') as f:
        d = {}
        for line in f:
            match = pat.match(line)
            s = int(match.group(1))
            ds = set([int(i) for i in match.group(2).split(",")])
            d[s] = ds
        return d


def part1(grid):
    done = set()
    todo = {0}
    while len(todo) > 0:
        s = todo.pop()
        done.add(s)
        todo |= grid[s] - done
    return len(done)

def part2(grid):
    done = set()
    groups = 0
    while len(set(grid.keys()) - done) > 0:
        groups += 1
        todo = {(set(grid.keys()) - done).pop()}
        while len(todo) > 0:
            s = todo.pop()
            done.add(s)
            todo |= grid[s] - done
    return groups


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
