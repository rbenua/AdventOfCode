#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(i) for i in f.read().split(",")]

def flip(ring, i, j):
    t = ring[i % len(ring)]
    ring[i % len(ring)] = ring[j % len(ring)]
    ring[j % len(ring)] = t

def part1(lengths, ring, cursor=0, skip=0):
    for length in lengths:
        for i in range(int(length / 2)):
            flip(ring, cursor + i, cursor + length - 1 - i)
        cursor = (cursor + length + skip) % len(ring)
        skip += 1
    return (ring[0] * ring[1], cursor, skip)

def xorall(l):
    t = 0
    for i in l:
        t ^= i
    return t

def part2(lengths):
    ring = list(range(256))
    cursor = 0
    skip = 0
    lengths += [17, 31, 73, 47, 23]
    for _ in range(64):
        (_, cursor, skip) = part1(lengths, ring, cursor, skip)

    result = bytes([xorall(ring[i*16:(i+1)*16]) for i in range(16)])
    return result.hex()


    


def run(filename):
    inp = read_input(filename)
    print(part1(inp, list(range(256)))[0])
    bs = open(filename, 'rb').read()
    print(part2(list(bs)))

if __name__ == "__main__":
    run(sys.argv[1])
