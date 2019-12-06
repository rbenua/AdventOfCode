#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        li = [tuple(l.strip() for l in line.split(")")) for line in f if line != "\n"]
        return {l[1]:l[0] for l in li}


        
def part1(inp):
    total = 0
    for i in inp.keys():
        n = 0
        while i != "COM":
            n += 1
            i = inp[i]
        total += n
    return total
            
    

def part2(inp):
    k1 = inp["SAN"]
    path1 = []
    while k1 != "COM":
        path1.append(k1)
        k1 = inp[k1]
    k2 = "YOU"
    #path1.reverse()
    print(path1)
    i = 0
    while k2 != "COM":
        if k2 in path1:
            return i + path1.index(k2) - 1
        i += 1
        k2 = inp[k2]
        


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
