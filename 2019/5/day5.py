#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(l) for l in f.read().split(",")]


def get_input_1():
    return 1
def get_input_5():
    return 5

def out_print(i):
    print("output:", i)

def execute(inp, in_fn, out_fn = out_print):
    pc = 0
    while True:
        if pc < 0 or pc >= len(inp):
            print("PC out of bounds", pc)
            return
        opcode = inp[pc] % 100
        modes = [int(c) for c in list("{:05}".format(int(inp[pc] / 100)))]
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
                return 0

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
            out_fn(get_arg(0))
            pc += 2
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
            print("illegal opcode!" + opcode)
            return
            
        
def part1(inp):
    execute(inp.copy(), get_input_1)

def part2(inp):
    execute(inp.copy(), get_input_5)

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
