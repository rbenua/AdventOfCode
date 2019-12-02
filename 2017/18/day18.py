#!/usr/bin/env python3

import sys, re, collections

#pat = re.compile("(\w+) (\w)( [a-z]|[\d-]+)?")

def read_input(filename):
    with open(filename, 'r') as f:
        prog = []
        for line in f:
            prog.append(line.split())
        return prog

def get_val(regs, oper):
    if oper.isalpha():
        return regs[oper]
    else:
        return int(oper)

EXITED = 0
WAITING = 1
RUNNING = 2

def step(regs, pc, prog, myqueue, otherqueue):
    if pc < 0 or pc >= len(prog):
        return (EXITED,)
    (insn, dest) = prog[pc][0:2]
    if insn == "snd":
        otherqueue.append(get_val(regs, dest))
        pc += 1
    elif insn == "set":
        oper = prog[pc][2]
        regs[dest] = get_val(regs, oper)
        pc += 1
    elif insn == "add":
        oper = prog[pc][2]
        regs[dest] += get_val(regs, oper)
        pc += 1
    elif insn == "mul":
        oper = prog[pc][2]
        regs[dest] *= get_val(regs, oper)
        pc += 1
    elif insn == "mod":
        oper = prog[pc][2]
        regs[dest] %= get_val(regs, oper)
        pc += 1
    elif insn == "rcv":
        if len(myqueue) == 0:
            return WAITING,
        regs[dest] = myqueue.pop(0)
        pc += 1
    elif insn == "jgz":
        oper = prog[pc][2]
        if get_val(regs, dest) != 0:
            pc += get_val(regs, oper)
        else:
            pc += 1
    else:
        print(f"unknown opcode {insn}")
        return (EXITED,)
    return (RUNNING, pc)
    
def part1(prog):
    regs = collections.defaultdict(int)
    pc = 0
    otherqueue = []
    while True:
        res = step(regs, pc, prog, [], otherqueue)
        if res[0] == EXITED:
            return
        elif res[0] == WAITING:
            if regs[prog[pc][1]] == 0:
                pc += 1
            else:
                return otherqueue[-1]
        elif res[0] == RUNNING:
            pc = res[1]

def part2(prog):
    regs0 = collections.defaultdict(int)
    regs1 = collections.defaultdict(int)
    regs1["p"] = 1
    queue0 = []
    queue1 = []
    pc0 = 0
    pc1 = 0
    done0 = False
    done1 = False
    sends = 0
    while not (done0 and done1):
        res0 = step(regs0, pc0, prog, queue0, queue1)
        res1 = step(regs1, pc1, prog, queue1, queue0)
        print(res0, res1, queue0, queue1, regs0, regs1)
        if res0[0] == RUNNING:
            pc0 = res0[1]
            done0 = False
        elif res0[0] == EXITED or len(queue0) == 0:
            done0 = True
            
        if res1[0] == RUNNING:
            if prog[pc1][0] == "snd":
                sends += 1
                if sends % 10000 == 0:
                    print(sends)
            pc1 = res1[1]
            done1 = False
        elif res1[0] == EXITED or len(queue1) == 0:
            done1 = True
    return sends



def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))


if __name__ == "__main__":
    run(sys.argv[1])
