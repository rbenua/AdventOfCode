#!/usr/bin/env python3

import sys, re

class TestCase:
    def __init__(self, before, insn, after):
        self.before = before
        self.insn = insn
        self.after = after

pat = re.compile("(\d+),? (\d+),? (\d+),? (\d+)")
tests = []

def read_input(filename):
    with open(filename, 'r') as f:
        line = f.readline()
        while line:
           match = pat.search(line)
           before = (int(match.group(1)), int(match.group(2)), int(match.group(3)), int(match.group(4)))
           match = pat.search(f.readline())
           insn = (int(match.group(1)), int(match.group(2)), int(match.group(3)), int(match.group(4)))
           match = pat.search(f.readline())
           after = (int(match.group(1)), int(match.group(2)), int(match.group(3)), int(match.group(4)))
           tests.append(TestCase(before, insn, after))
           f.readline()
           line = f.readline()

def addr(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,b,c) > 3:
        return None
    regs[c] = regs[a] + regs[b]
    return tuple(regs)

def addi(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = regs[a] + b
    return tuple(regs)

def mulr(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,b,c) > 3:
        return None
    regs[c] = regs[a] * regs[b]
    return tuple(regs)

def muli(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = regs[a] * b
    return tuple(regs)

def banr(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,b,c) > 3:
        return None
    regs[c] = regs[a] & regs[b]
    return tuple(regs)

def bani(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = regs[a] & b
    return tuple(regs)

def borr(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,b,c) > 3:
        return None
    regs[c] = regs[a] | regs[b]
    return tuple(regs)

def bori(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = regs[a] | b
    return tuple(regs)

def setr(inp, insn):
    regs = list(inp)
    (_, a, _, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = regs[a]
    return tuple(regs)

def seti(inp, insn):
    regs = list(inp)
    (_, a, _, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = a
    return tuple(regs)

def gtir(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(b,c) > 3:
        return None
    regs[c] = (0,1)[a > regs[b]]
    return tuple(regs)

def gtri(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = (0,1)[regs[a] > b]
    return tuple(regs)

def gtrr(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,b,c) > 3:
        return None
    regs[c] = (0,1)[regs[a] > regs[b]]
    return tuple(regs)

def eqir(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(b,c) > 3:
        return None
    regs[c] = (0,1)[a == regs[b]]
    return tuple(regs)

def eqri(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,c) > 3:
        return None
    regs[c] = (0,1)[regs[a] == b]
    return tuple(regs)

def eqrr(inp, insn):
    regs = list(inp)
    (_, a, b, c) = insn
    if max(a,b,c) > 3:
        return None
    regs[c] = (0,1)[regs[a] == regs[b]]
    return tuple(regs)

fns = [addi, addr, muli, mulr, bani, banr, bori, borr, seti, setr, gtir, gtri, gtrr, eqir, eqri, eqrr]

def count(t):
    possible = [f for f in fns if f(t.before, t.insn) == t.after]
    #print(possible)
    return len(possible)

def run(filename):
    read_input(filename)
    print(len([1 for t in tests if count(t) >= 3]))

if __name__ == "__main__":
    run(sys.argv[1])
