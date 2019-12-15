#!/usr/bin/env python3

import sys, re, collections, math

def read_input(filename):
    return read_intcode(filename)

def read_intcode(filename):
    with open(filename, 'r') as f:
        l = [int(l) for l in f.read().split(",")]
        l = zip(range(len(l)), l)
        c = collections.defaultdict(int)
        for (k,v) in l:
            c[k] = v
        return c

def const(val):
    return lambda: val

def out_print(i):
    print("output:", i)

def execute(inp, in_fn, out_fn = out_print, stop_on_output = False, pc = 0, relbase = 0):
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
            elif mode == 2:
                return inp[relbase + arg]
            else:
                print("Invalid input mode", mode)
                return 0
        
        def set_arg(offset, val):
            mode = modes[offset]
            arg = inp[pc + offset + 1]
            if mode == 0:
                inp[arg] = val
            elif mode == 2:
                inp[relbase + arg] = val
            else:
                print("Invalid output mode", mode)

        if opcode == 99:
            return
        elif opcode == 1:
            set_arg(2, get_arg(0) + get_arg(1))
            pc += 4
        elif opcode == 2:
            set_arg(2, get_arg(0) * get_arg(1))
            pc += 4
        elif opcode == 3:
            set_arg(0, in_fn())
            pc += 2
        elif opcode == 4:
            val = get_arg(0)
            pc += 2
            if stop_on_output:
                return (val, pc, relbase)
            else:
                out_fn(val)
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
        elif opcode == 9:
            relbase += get_arg(0)
            pc += 2

        else:
            print("illegal opcode!", opcode)
            return
            
def neighbors(here):
    x, y = here
    return ((1, (x, y-1)), (2, (x, y+1)), (3, (x-1, y)), (4, (x+1, y)))

def part1(inp):
    explored = {(0, 0)}
    to_expand = [(0, (0, 0), inp.copy(), 0, 0)]
    while True:
        (dist, here, prog, pc, rb) = to_expand.pop(0)
        for (cmd, (newx, newy)) in neighbors(here):
            newpt = (newx, newy)
            if newpt in explored:
                continue
            newprog = prog.copy()
            (res, newpc, newrb) = execute(newprog, const(cmd), out_print, True, pc, rb)
            if res == 2:
                return (dist + 1, newpt, newprog, newpc, newrb)
            explored.add(newpt)
            if res == 1:
                to_expand.append((dist +1, newpt, newprog, newpc, newrb))


def part2(inp):
    explored = {inp[1]}
    to_expand = [(0, *inp[1:])]
    while True:
        (dist, here, prog, pc, rb) = to_expand.pop(0)
        for (cmd, (newx, newy)) in neighbors(here):
            newpt = (newx, newy)
            if newpt in explored:
                continue
            newprog = prog.copy()
            (res, newpc, newrb) = execute(newprog, const(cmd), out_print, True, pc, rb)
            explored.add(newpt)
            if res != 0:
                to_expand.append((dist +1, newpt, newprog, newpc, newrb))
        if len(to_expand) == 0:
            return dist

def run(filename):
    inp = read_input(filename)
    st = part1(inp)
    print(st[0], st[1])
    print(part2(st))

if __name__ == "__main__":
    run(sys.argv[1])
