#!/usr/bin/env python3

import sys, re, collections

pat = re.compile("(\w+) (\w+) ([-\d]+) if (\w+) (.+)\n")

def read_input(filename):
    with open(filename, 'r') as f:
        insns = []
        for line in f:
            match = pat.match(line)
            dest = match.group(1)
            inc = 1
            if match.group(2) == "dec":
                inc = -1
            amt = int(match.group(3))
            cr = match.group(4)
            cond = match.group(5)
            insns.append((dest, inc, amt, cr, cond))
        return insns

def part1(insns):
    regs = collections.defaultdict(int)
    for (dest, inc, amt, cr, cond) in insns:
        if eval('regs["{}"] {}'.format(cr, cond)):
            regs[dest] += amt * inc
    return max(regs.values())

def part2(insns):
    regs = collections.defaultdict(int)
    maxs = collections.defaultdict(int)
    for (dest, inc, amt, cr, cond) in insns:
        if eval('regs["{}"] {}'.format(cr, cond)):
            regs[dest] += amt * inc
            maxs[dest] = max(maxs[dest], regs[dest])
    return max(maxs.values())

def run(filename):
    insns = read_input(filename)
    print(part1(insns))
    print(part2(insns))

if __name__ == "__main__":
    run(sys.argv[1])
