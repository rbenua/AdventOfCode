#!/usr/bin/env python3

import sys, re

class TestCase:
    def __init__(self, before, insn, after):
        self.before = before
        self.insn = insn
        self.after = after

pat = re.compile("(\d+),? (\d+),? (\d+),? (\d+)")
tests = []
program = []

def read_tests(filename):
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

def read_program(filename):
    with open(filename,'r') as f:
        for line in f:
            match = pat.search(line)
            insn = (int(match.group(1)), int(match.group(2)), int(match.group(3)), int(match.group(4)))
            program.append(insn)

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

opcodes = [fns] * 16

def attempt(t):
    code = t.insn[0]
    if len(opcodes[code]) == 1:
        return
    opcodes[code] = [f for f in opcodes[code] if f(t.before, t.insn) == t.after]

def reduce():
    global opcodes
    iters = 1
    while max([len(c) for c in opcodes]) > 1:
        for l in opcodes:
            if len(l) == 1:
                for other in opcodes:
                    if l != other:
                        try:
                            other.remove(l[0])
                        except ValueError:
                            pass
        print("Pass", iters, "lengths", [len(c) for c in opcodes])
        iters += 1
    opcodes = [c[0] for c in opcodes]

def simulate():
    state = (0,0,0,0)
    for insn in program:
        state = opcodes[insn[0]](state, insn)
    return state

def run(testfile, progfile):
    read_tests(testfile)
    for t in tests:
        attempt(t)
    reduce()
    print(opcodes)
    read_program(progfile)
    print(simulate())

if __name__ == "__main__":
    run(sys.argv[1], sys.argv[2])
