#!/usr/bin/env python3

import sys, re, collections, math

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(c) for c in f.read().strip()]

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
        
def sequence(write_idx, read_idx):
    return [0, 1, 0, -1][((read_idx + 1) // (write_idx + 1)) % 4]


def part1(inp):
    for iteration in range(100):
        print(iteration)
        out = [0] * len(inp)
        for write_idx in range(len(out)):
            val = 0
            for read_idx in range(len(inp)):
                val += inp[read_idx] * sequence(write_idx, read_idx)
            out[write_idx] = abs(val) % 10
        inp = out

    return "".join(map(str, inp[0:8]))

def part2(inp):
    offset = int("".join(map(str, inp[0:7])))
    inp = (inp * 10000)[offset:]
    for iteration in range(100):
        print(iteration)
        out = [0] * len(inp)
        write_idx = len(inp) - 1
        prev_write = 0
        while write_idx >= 0:
            out[write_idx] = (inp[write_idx] + prev_write) % 10
            prev_write = out[write_idx]
            write_idx -= 1
        inp = out
    return "".join(map(str, inp[0:8]))


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
