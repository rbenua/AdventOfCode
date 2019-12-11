#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
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
    colors = collections.defaultdict(int)
    visited = set()
    location = (0, 0)
    facing = 0
    painted = False
    def input_paint():
        return colors[location]
    def output_paint(val):
        nonlocal location, facing, painted
        if not painted:
            visited.add(location)
            colors[location] = val
            painted = True
        else:
            if val == 0:
                facing = (facing - 1) % 4
            else:
                facing = (facing + 1) % 4
            delta = [(0, 1), (1, 0), (0, -1), (-1, 0)][facing]
            location = (location[0] + delta[0], location[1] + delta[1])
            painted = False

    execute(inp.copy(), input_paint, output_paint)
    return len(visited)

def print_grid(colors):
    keys = colors.keys()
    miny = min(y for (x, y) in keys)
    minx = min(x for (x, y) in keys)
    maxy = max(y for (x, y) in keys)
    maxx = max(x for (x, y) in keys)
    for y in range(miny, maxy + 1):
        print("".join([" #"[colors[(x, y)]] for x in range(minx, maxx + 1)]))
    

def part2(inp):
    colors = collections.defaultdict(int)
    colors[(0, 0)] = 1
    visited = set()
    location = (0, 0)
    facing = 2
    painted = False
    def input_paint():
        return colors[location]
    def output_paint(val):
        nonlocal location, facing, painted
        if not painted:
            visited.add(location)
            colors[location] = val
            painted = True
        else:
            if val == 0:
                facing = (facing + 1) % 4
            else:
                facing = (facing - 1) % 4
            delta = [(0, 1), (1, 0), (0, -1), (-1, 0)][facing]
            location = (location[0] + delta[0], location[1] + delta[1])
            painted = False

    execute(inp.copy(), input_paint, output_paint)
    print_grid(colors)
    return len(visited)

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
