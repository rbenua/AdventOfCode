#!/usr/bin/env python3

import sys, re

pat = re.compile("([a-z]+),? (\d+),? (\d+),? (\d+)")


def read_input(filename):
    program = []
    ipreg = 0
    with open(filename, 'r') as f:
        #i = 0
        #a = open("annot.txt","w")
        for line in f:
            if line[0] == '#':
                ipreg = int(line[-2])
                print("setting ipreg", ipreg)
            else:
                match = pat.search(line)
                program.append((eval(match.group(1)), int(match.group(2)), int(match.group(3)), int(match.group(4))))
                #a.write("{0:2}: {1} {2[1]:8} {2[2]:8} {2[3]}\n".format(i, match.group(1), program[-1]))
                #i += 1
                if not match:
                    raise ValueError
    #a.close()
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

def divi(regs, insn):
    (_, a, b, c) = insn
    if max(a,c) > 5:
        return None
    regs[c] = int(regs[a] / b)

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

seen = set()
dups = 0
prev = 0
def step(regs, program, ipreg):
    global dups, prev
    insn = program[regs[ipreg]]
    dbg = regs[ipreg] == 19
    if dbg:
        pass
        #print("ip", regs[ipreg], end=' ')
        #print(regs, end=' ')
    if dbg:
        pass
        #print(insn, end=' ')
    insn[0](regs, insn)
    if dbg:
        #print(regs)
        if regs[4] in seen:
            print(prev)
            exit(0)
            dups += 1
            if (dups % 100) == 0:
                print("found", dups, "dups,", len(seen),"uniques")
        else:
            #print("{}".format(regs[4]))
            seen.add(regs[4])
            prev = regs[4]
        if len(seen) % 1000 == 0:
            print(len(seen))
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
