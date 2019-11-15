#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(w) for w in f.read().split()]

def rebalance(buckets):
    to_distribute = max(buckets)
    idx = (buckets.index(to_distribute) + 1) % len(buckets)
    buckets[idx - 1] = 0
    while to_distribute > 0:
        buckets[idx] += 1
        idx = (idx + 1) % len(buckets)
        to_distribute -= 1

def part1(buckets):
    iterations = 0
    seen = {tuple(buckets)}
    while 1:
        rebalance(buckets)
        iterations += 1
        t = tuple(buckets)
        if t in seen:
            return iterations
        seen.add(t)

def part2(buckets):
    iterations = 0
    seen = {tuple(buckets):0}
    while 1:
        rebalance(buckets)
        iterations += 1
        t = tuple(buckets)
        if t in seen:
            return iterations - seen[t]
        seen[t] = iterations

def run(filename):
    buckets = read_input(filename)
    print(part1(buckets))
    buckets = read_input(filename)
    print(part2(buckets))

if __name__ == "__main__":
    run(sys.argv[1])
