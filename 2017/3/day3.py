#!/usr/bin/env python3

import sys, re, collections


def read_input(filename):
    with open(filename, 'r') as f:
        return [[int(i) for i in line.split()] for line in f]

def traverse(loc):
    minX = 0
    maxX = 0
    minY = 0
    maxY = 0
    cursor = 1
    while 1:
        #go right
        maxX += 1
        #print("c:{} x:{}-{} y:{}-{}".format(cursor, minX, maxX, minY, maxY))
        if (loc - cursor) <= (maxX - minX):
            return ((minX + loc - cursor), minY)
        cursor += (maxX - minX)

        #go up
        maxY += 1
        #print("c:{} x:{}-{} y:{}-{}".format(cursor, minX, maxX, minY, maxY))
        if (loc - cursor) <= (maxY - minY):
            return (maxX, (minY + loc - cursor))
        cursor += (maxY - minY)

        #go left
        minX -= 1
        #print("c:{} x:{}-{} y:{}-{}".format(cursor, minX, maxX, minY, maxY))
        if (loc - cursor) <= (maxX - minX):
            return ((maxX - loc + cursor), maxY)
        cursor += (maxX - minX)

        #go down
        minY -= 1
        #print("c:{} x:{}-{} y:{}-{}".format(cursor, minX, maxX, minY, maxY))
        if (loc - cursor) <= (maxY - minY):
            return (minX, (maxY - loc + cursor))
        cursor += (maxY - minY)




def part1(loc):
    (x, y) = traverse(loc)
    print("({}, {})".format(x, y))
    return abs(x) + abs(y)

def sum_nbrs(t, d):
    (x, y) = t
    return d[(x-1, y-1)] + d[(x-1, y)] + d[(x-1, y+1)] + d[(x, y-1)] + d[(x, y+1)] + d[(x+1, y-1)] + d[(x+1, y)] + d[(x+1, y+1)]

def part2(loc):
    d = collections.defaultdict(int)
    d[(0, 0)] = 1
    i = 2
    while 1:
        t = traverse(i)
        d[t] = sum_nbrs(t, d)
        print("{}: {}, {}".format(i, t, d[t]))
        if d[t] > loc:
            return d[t]
        i += 1


def run(loc):
    print(part1(int(loc)))
    print(part2(int(loc)))

if __name__ == "__main__":
    run(sys.argv[1])
