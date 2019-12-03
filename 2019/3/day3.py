#!/usr/bin/env python3

import sys, re, collections

#pat = re.compile("(\w+) (\w)( [a-z]|[\d-]+)?")

def read_input(filename):
    with open(filename, 'r') as f:
        return (f.readline().split(","), f.readline().split(","))

def part1(inp):
    visited = set()
    pos = (0,0)
    for line in inp[0]:
        direction = [(0,1), (1,0), (0,-1), (-1,0)]["URDL".index(line[0])]
        for _ in range(int(line[1:])):
            pos = (pos[0] + direction[0], pos[1] + direction[1])
            visited.add(pos)

    pos = (0,0)
    intersections = []
    for line in inp[1]:
        direction = [(0,1), (1,0), (0,-1), (-1,0)]["URDL".index(line[0])]
        for _ in range(int(line[1:])):
            pos = (pos[0] + direction[0], pos[1] + direction[1])
            if pos in visited:
                intersections.append(pos)
    return min([abs(x) + abs(y) for (x, y) in intersections])

def part2(inp):
    visited = {}
    pos = (0,0)
    steps = 0
    for line in inp[0]:
        direction = [(0,1), (1,0), (0,-1), (-1,0)]["URDL".index(line[0])]
        for _ in range(int(line[1:])):
            pos = (pos[0] + direction[0], pos[1] + direction[1])
            steps += 1
            visited[pos] = steps

    pos = (0,0)
    steps = 0
    intersections = set()
    for line in inp[1]:
        direction = [(0,1), (1,0), (0,-1), (-1,0)]["URDL".index(line[0])]
        for _ in range(int(line[1:])):
            pos = (pos[0] + direction[0], pos[1] + direction[1])
            steps += 1
            if pos in visited:
                intersections.add(steps + visited[pos])
    return min(intersections)

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    run(sys.argv[1])
