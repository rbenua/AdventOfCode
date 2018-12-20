#!/usr/bin/env python3

import sys, re

pat = re.compile("([a-z]+),? (\d+),? (\d+),? (\d+)")


def read_input(filename):
    program = []
    ipreg = 0
    with open(filename, 'r') as f:
        i = 0
        for line in f:
            if line[0] == '#':
                ipreg = int(line[-2])
                print("setting ipreg", ipreg)
            else:
                match = pat.search(line)
                if not match:
                    raise ValueError
    return (program, ipreg)

def addr(regs, insn):
    (_, a, b, c) = insn
    if max(a,b,c) > 5:
        return None
    regs[c] = regs[a] + regs[b]

def addi(regs, insn):
    (_, a, b, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = regs[a] + b

def mulr(regs, insn):
    (_, a, b, c) = insn
    if max(a,b,c) > 5:
        return None
    regs[c] = regs[a] * regs[b]

def muli(regs, insn):
    (_, a, b, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = regs[a] * b

def banr(regs, insn):
    (_, a, b, c) = insn
    if max(a,b,c) > 5:
        return None
    regs[c] = regs[a] & regs[b]

def bani(regs, insn):
    (_, a, b, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = regs[a] & b

def borr(regs, insn):
    (_, a, b, c) = insn
    if max(a,b,c) > 5:
        return None
    regs[c] = regs[a] | regs[b]

def bori(regs, insn):
    (_, a, b, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = regs[a] | b

def setr(regs, insn):
    (_, a, _, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = regs[a]

def seti(regs, insn):
    (_, a, _, c) = insn
    if c > 5:
        return None
    regs[c] = a

def gtir(regs, insn):
    (_, a, b, c) = insn
    if max(b,c) > 5:
        return None
    regs[c] = (0,1)[a > regs[b]]

def gtri(regs, insn):
    (_, a, b, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = (0,1)[regs[a] > b]

def gtrr(regs, insn):
    (_, a, b, c) = insn
    if max(a,b,c) > 5:
        return None
    regs[c] = (0,1)[regs[a] > regs[b]]

def eqir(regs, insn):
    (_, a, b, c) = insn
    if max(b,c) > 5:
        return None
    regs[c] = (0,1)[a == regs[b]]

def eqri(regs, insn):
    (_, a, b, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = (0,1)[regs[a] == b]

def eqrr(regs, insn):
    (_, a, b, c) = insn
    if max(a,b,c) > 5:
        return None
    regs[c] = (0,1)[regs[a] == regs[b]]


def step(regs, program, ipreg):
    insn = program[regs[ipreg]]
    #dbg = insn[3] == 0
    dbg = True
    if dbg:
        print("ip", regs[ipreg], end=' ')
        print(regs, end=' ')
    if dbg:
        print(insn, end=' ')
    eval(insn[0])(regs, insn)
    if dbg:
        print(regs)
    regs[ipreg] += 1
    
    

def run(filename):
    (program, ipreg) = read_input(filename)
    print("ipreg",ipreg)
    regs = [0,0,0,0,0,0]
    while regs[ipreg] < len(program):
        step(regs, program, ipreg)
    print(regs)

if __name__ == "__main__":
    run(sys.argv[1])
