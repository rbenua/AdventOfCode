#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(i) for i in f.read().split(",")]

def flip(ring, i, j):
    t = ring[i % len(ring)]
    ring[i % len(ring)] = ring[j % len(ring)]
    ring[j % len(ring)] = t

def round(lengths, ring, cursor=0, skip=0):
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

def knothash(lengths):
    ring = list(range(256))
    cursor = 0
    skip = 0
    lengths += [17, 31, 73, 47, 23]
    for _ in range(64):
        (_, cursor, skip) = round(lengths, ring, cursor, skip)

    result = bytes([xorall(ring[i*16:(i+1)*16]) for i in range(16)])
    return result

def popcount(bs):
    return sum([bin(i).count('1') for i in bs])

def part1(inp):
    total = 0
    hashes = []
    for i in range(128):
        bs = f"{inp}-{i}".encode("ascii")
        ch = knothash(list(bs))
        total += popcount(ch)
        hashes.append(ch)
    return (total, hashes)

def bs2bools(bs):
    l = []
    for i in bs:
        l += [bool(int(b)) for b in f"{i:08b}"]
    return l

def addnbrs(x, y, bools, todo):
    if x < 127 and bools[x+1][y]:
        todo.add((x+1, y))
    if x > 0 and bools[x-1][y]:
        todo.add((x-1, y))
    if y < 127 and bools[x][y+1]:
        todo.add((x, y+1))
    if y > 0 and bools[x][y-1]:
        todo.add((x, y-1))

def part2(hashes):
    bools = [bs2bools(bs) for bs in hashes]
    print(len(bools), len(bools[0]))
    count = 0
    for x in range(128):
        for y in range(128):
            if bools[x][y]:
                count += 1
                todo = {(x, y)}
                while len(todo) > 0:
                    (cx, cy) = todo.pop()
                    bools[cx][cy] = False
                    addnbrs(cx, cy, bools, todo)

    return count

def run(filename):
    (total, hashes) = part1(filename)
    print(total)
    print(part2(hashes))


if __name__ == "__main__":
    run(sys.argv[1])
