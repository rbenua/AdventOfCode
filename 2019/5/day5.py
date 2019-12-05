#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(l) for l in f.read().split(",")]


def get_input():
    return 5

def set_output(i):
    print("output:", i)

def get_arg(inp, mode, arg):
    if mode == 0:
        return inp[arg]
    elif mode == 1:
        return arg
    
def execute(inp):
    pc = 0
    while True:
        opcode = inp[pc] % 100
        modes = [int(c) for c in list("{:05}".format(int(inp[pc] / 100)))]
        modes.reverse()
        print(pc, inp[pc], opcode, modes)
        if pc < 0 or pc >= len(inp) or opcode == 99:
            return
        elif opcode == 1:
            inp[inp[pc+3]] = get_arg(inp, modes[0], inp[pc+1]) + get_arg(inp, modes[1], inp[pc+2])
            pc += 4
        elif opcode == 2:
            inp[inp[pc+3]] = get_arg(inp, modes[0], inp[pc+1]) * get_arg(inp, modes[1], inp[pc+2])
            pc += 4
        elif opcode == 3:
            inp[inp[pc+1]] = get_input()
            pc += 2
        elif opcode == 4:
            set_output(get_arg(inp, modes[0], inp[pc+1]))
            pc += 2
        elif opcode == 5:
            if get_arg(inp, modes[0], inp[pc+1]) != 0:
                pc = get_arg(inp, modes[1], inp[pc+2])
            else:
                pc += 3
        elif opcode == 6:
            if get_arg(inp, modes[0], inp[pc+1]) == 0:
                pc = get_arg(inp, modes[1], inp[pc+2])
            else:
                pc += 3
        elif opcode == 7:
            if get_arg(inp, modes[0], inp[pc+1]) < get_arg(inp, modes[1], inp[pc+2]):
                inp[inp[pc+3]] = 1
            else:
                inp[inp[pc+3]] = 0
            pc += 4
        elif opcode == 8:
            if get_arg(inp, modes[0], inp[pc+1]) == get_arg(inp, modes[1], inp[pc+2]):
                inp[inp[pc+3]] = 1
            else:
                inp[inp[pc+3]] = 0
            pc += 4

        else:
            print("illegal opcode!" + opcode)
            return
            
        
def part1(inp):
    execute(inp)
    pass

def part2(inp):
    pass

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    inp = read_input(filename)
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
