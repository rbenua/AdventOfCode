#!/usr/bin/env python3

import sys, re, collections, itertools

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(l) for l in f.read().strip()]

WIDTH = 25
HEIGHT = 6
layers = []
image = [["2" for _ in range(WIDTH)] for _ in range(HEIGHT)]

def part1(inp):
    while inp != []:
        counts = [0,0,0]
        for y in range(HEIGHT):
            for x in range(WIDTH):
                counts[inp.pop()] += 1
                    
        layers.append((counts[0], counts[1] * counts[2]))
    return min(layers)

def part2(inp):
    while inp != []:
        for y in range(HEIGHT):
            for x in range(WIDTH):
                if image[y][x] == "2":
                    image[y][x] = str(inp.pop(0))
                else:
                    inp.pop(0)
    for y in range(HEIGHT):
        print("".join(image[y]).replace("1", " "))
    for y in range(HEIGHT):
        print("".join(image[y]).replace("0", " "))
    pass

def run(filename):
    inp = read_input(filename)
    print(part1(inp.copy()))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
