#!/usr/bin/env python3

import sys

class Node:
    def __init__(self):
        self.children = []
        self.metadata = []

with open(sys.argv[1], 'r') as f:
    inp = [int(i) for i in f.read().split(" ")]

def parse(inp):
    res = Node()
    childcount = inp[0]
    datacount = inp[1]
    idx = 2
    for _ in range(childcount):
        child, clen = parse(inp[idx:])
        res.children.append(child)
        idx += clen

    res.metadata = inp[idx:idx + datacount]
    return res, idx + datacount

def total(n):
    return sum(n.metadata) + sum([total(c) for c in n.children])

def pretty(n, indent):
    print(indent * " ", n.metadata)
    for c in n.children:
        pretty(c, indent + 1)

pretty(parse(inp)[0], 0)
print(total(parse(inp)[0]))
