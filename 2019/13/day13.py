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

def get_input_1():
    return 1
def get_input_2():
    return 2

def out_print(i):
    print("output:", i)

def execute(inp, in_fn, out_fn = out_print):
    pc = 0
    relbase = 0
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
        elif opcode == 9:
            relbase += get_arg(0)
            pc += 2

        else:
            print("illegal opcode!", opcode)
            return
            

def part1(inp):
    blocks = set()
    x = 0
    y = 0
    parity = 0
    def out_blocks(val):
        nonlocal blocks, parity, x, y
        if parity == 0:
            x = val
        elif parity == 1:
            y = val
        elif val == 2:
            blocks.add((x, y))
        parity = (parity + 1) % 3
    execute(inp.copy(), get_input_1, out_blocks)
    return len(blocks)

def print_grid(blocks, score, maxx, maxy):
    for y in range(maxy+1):
        print("".join([" #@=*"[blocks[(x,y)]] for x in range(maxx+1)]))
    print(score)

def part2(inp):
    inp[0] = 2
    x = 0
    y = 0
    maxx, maxy = (0, 0)
    parity = 0
    blocks = collections.defaultdict(int)
    score = 0
    ballx = 0
    bally = 0
    paddlex = 0
    paddley = 0
    balldir = 1
    def out_blocks(val):
        nonlocal blocks, parity, x, y, score, maxx, maxy, ballx, bally, paddlex, paddley, balldir
        if parity == 0:
            x = val
        elif parity == 1:
            y = val
        elif (x, y) == (-1, 0):
            score = val
        else:
            blocks[(x, y)] = val
            maxx, maxy = max(x, maxx), max(y, maxy)
            if val == 4:
                balldir = x - ballx
                ballx = x
                bally = y
            elif val == 3:
                paddley = y
                paddlex = x
        parity = (parity + 1) % 3
    def in_controls():
        print_grid(blocks, score, maxx, maxy)
        #print(ballx, balldir, paddlex)
        if paddlex < ballx:
            return 1
        elif paddlex > ballx:
            return -1
        elif bally == paddley - 1:
            return 0
        else:
            return balldir
    execute(inp, in_controls, out_blocks)
    return score

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
