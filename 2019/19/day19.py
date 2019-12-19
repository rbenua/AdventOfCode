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
    print(f"output: {i}")

def execute(inp, in_gen, out_fn = out_print, stop_on_output = False, pc = 0, relbase = 0):
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
            set_arg(0, in_gen.__next__())
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
    total = 0
    grid = []
    def input_coords():
        for y in range(50):
            for x in range(50):
                yield x
                yield y
    in_gen = input_coords()
    def out_count(val):
        nonlocal total
        total += val
        grid.append(val)
    for _ in range(50 * 50):
        execute(inp.copy(), in_gen, out_count)

    for y in range(50):
        print("".join(str(c) for c in grid[y*50:(y+1)*50]))
    return total


def part2_orig(inp):
    def in_coords(x, y):
        yield x
        yield y

    memo = {}
    def test(x, y):
        if (x, y) not in memo:
            #print("tested", y, x, end=" ")
            memo[(x, y)] = execute(inp.copy(), in_coords(x, y), in_coords, True)[0] == 1
            #print(memo[(x, y)])
        return memo[(x, y)]
    
    startx = 0
    starty = 20
    done = False
    while not done:
        while not test(startx, starty):
            startx += 1
        print(starty, startx)
        thisx = startx
        while test(thisx, starty):
            if test(thisx + 99, starty) and test(thisx, starty + 99) and test(thisx + 99, starty + 99):
                return thisx * 10000 + starty 
            thisx += 1
        starty += 1
    return startx * 10000 + starty
        
def part2_fast(inp):
    def in_coords(x, y):
        yield x
        yield y

    memo = {}
    def test(x, y):
        if (x, y) not in memo:
            #print("tested", y, x, end=" ")
            memo[(x, y)] = execute(inp.copy(), in_coords(x, y), in_coords, True)[0] == 1
            #print(memo[(x, y)])
        return memo[(x, y)]
    
    startx = 0
    starty = 20
    while True:
        while not test(startx, starty):
            startx += 1
        print(starty, startx)
        if test(startx + 99, starty - 99):
            return startx * 10000 + starty - 99
        starty += 1

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2_fast(inp))

if __name__ == "__main__":
    run(sys.argv[1])
