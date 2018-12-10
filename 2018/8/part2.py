#!/usr/bin/env python3

import sys

with open(sys.argv[1], 'r') as f:
    inp = [int(i) for i in f.read().split(" ")]

class Node:
    def __init__(self):
        self.children = []
        self.metadata = []

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

def value(n):
    if len(n.children) == 0:
        return sum(n.metadata)
    else:
        return sum([value(n.children[c - 1]) for c in n.metadata if c < len(n.children) + 1])

def depth(n):
    return 1 + max([depth(c) for c in n.children] + [0])

#pretty(parse(inp)[0], 0)
print(value(parse(inp)[0]))
print(depth(parse(inp)[0]))

