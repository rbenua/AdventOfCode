#!/usr/bin/env python3

import sys, re, collections, itertools

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(l) for l in f.read().split(",")]


def get_input_1():
    return 1
def get_input_5():
    return 5

def out_print(i):
    print("output:", i)

def execute(inp, in_fn, out_fn = out_print, pc = 0, stop_on_input = False):
    while True:
        if pc < 0 or pc >= len(inp):
            print("PC out of bounds", pc)
            return
        opcode = inp[pc] % 100
        modes = [int(c) for c in list("{:010}".format(int(inp[pc] / 100)))]
        modes.reverse()
        def get_arg(offset):
            mode = modes[offset]
            arg = inp[pc + offset + 1]
            if mode == 0:
                return inp[arg]
            elif mode == 1:
                return arg
            else:
                print("Invalid input mode", mode)
                return 0
        
        def set_arg(offset, val):
            mode = modes[offset]
            arg = inp[pc + offset + 1]
            if mode == 0:
                inp[arg] = val
            else:
                print("Invalid output mode", mode)

        if opcode == 99:
            return (True, 0)
        elif opcode == 1:
            set_arg(2, get_arg(0) + get_arg(1))
            pc += 4
        elif opcode == 2:
            set_arg(2, get_arg(0) * get_arg(1))
            pc += 4
        elif opcode == 3:
            if stop_on_input:
                return (False, pc)
            set_arg(0, in_fn())
            pc += 2
        elif opcode == 4:
            out_fn(get_arg(0))
            pc += 2
            return (False, pc)
        elif opcode == 5:
            if get_arg(0) != 0:
                pc = get_arg(1)
            else:
                pc += 3
        elif opcode == 6:
            if get_arg(0) == 0:
                pc = get_arg(1)
            else:
                pc += 3
        elif opcode == 7:
            if get_arg(0) < get_arg(1):
                set_arg(2, 1)
            else:
                set_arg(2, 0)
            pc += 4
        elif opcode == 8:
            if get_arg(0) == get_arg(1):
                set_arg(2, 1)
            else:
                set_arg(2, 0)
            pc += 4

        else:
            print("illegal opcode!", opcode)
            return
            
        
def part1(inp):
    best = 0
    for perm in itertools.permutations(list(range(5))):
        val = 0
        for i in perm:
            snd = False
            def out_val(v):
                nonlocal val
                val = v
            def in_val():
                nonlocal snd 
                if not snd:
                    snd = True
                    return i
                else:
                    return val
            execute(inp.copy(), in_val, out_val)
            best = max(best, val)
    return best
        
    

def part2(inp):
    best = 0
    for perm in itertools.permutations(list(range(5, 10))):
        val = 0
        snd = [False] * 5
        pcs = [0] * 5
        progs = [inp.copy() for _ in range(5)]
        def out_val(v):
            nonlocal val
            val = v
        done = False 
        while not done:
            for i in range(5):
                #print("running machine", i, val, snd, pcs)
                def in_val():
                    nonlocal snd
                    if not snd[i]:
                        snd[i] = True
                        return perm[i]
                    else:
                        return val
                #run until first output
                (ret, pcs[i]) = execute(progs[i], in_val, out_val, pcs[i], False)
                #run until next input (or halt)
                (ret, pcs[i]) = execute(progs[i], in_val, out_val, pcs[i], True)
                if i == 4 and ret:
                    print(perm, val)
                    best = max(best, val)
                    done = True
                    break
    return best

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
