#!/usr/bin/env python3

import sys, re, collections

reg = re.compile("x(\d+)/(\d+)")

def read_input(filename):
    with open(filename, 'r') as f:
        return f.read().strip().split(',')

def swap(l, i, j):
    t = l[i]
    l[i] = l[j]
    l[j] = t

def part1(inp, l):
    for cmd in inp:
        if cmd[0] == 's':
            brk = int(cmd[1:]) * -1
            l = l[brk:] + l[:brk]
        elif cmd[0] == 'p':
            swap(l, l.index(cmd[1]), l.index(cmd[3]))
        elif cmd[0] == 'x':
            match = reg.match(cmd)
            swap(l, int(match.group(1)), int(match.group(2)))
    return l



def part2(inp):
    orig_l = list("abcdefghijklmnop")
    l = list("abcdefghijklmnop")
    seen = [tuple(l)]
    i = 0
    while True:
        l = part1(inp, l)
        if l == orig_l:
            print("cycle length", i)
            return ''.join(seen[1000000000 % len(seen)])
        seen.append(tuple(l))
        i += 1

def run(filename):
    l = list("abcdefghijklmnop")
    inp = read_input(filename)
    print(''.join(part1(inp, l)))
    print(part2(inp))


if __name__ == "__main__":
    run(sys.argv[1])
