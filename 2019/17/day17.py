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

def printgrid(scaffolds):
    xs = [x for (x, y) in scaffolds]
    ys = [y for (x, y) in scaffolds]
    minx, maxx = min(xs), max(xs)
    miny, maxy = min(ys), max(ys)

    for y in range(miny, maxy + 1):
        print("".join([".#>"[((x, y) in scaffolds) + ((x, y) == (32, 18))] for x in range(minx, maxx + 1)]))

def part1(inp):
    x, y = 0, 0
    scaffolds = set()
    def out_map(val):
        nonlocal x, y, scaffolds
        if val == 35 or val == 118 or val == 94 or val == 62 or val == 60:
            if val != 35:
                print(val, x, y)
            scaffolds.add((x, y))
        elif val == 10:
            x = 0
            y += 1
            return
        elif val == 46 or val == 88:
            pass
        else:
            print("Weird output", val)
        x += 1
    
    execute(inp.copy(), const(1), out_map)

    total = 0
    for pt in scaffolds:
        intersection = True
        for (_, pt2) in neighbors(pt):
            if pt2 not in scaffolds:
                intersection = False
        if intersection:
            total += pt[0] * pt[1]
    printgrid(scaffolds)
    return total




def part2(inp):
    main = "A,B,B,A,B,C,A,C,B,C\n"
    pathA = "L,4,L,6,L,8,L,12\n"
    pathB = "L,8,R,12,L,12\n"
    pathC = "R,12,L,6,L,6,L,8\n"
    ack = "n\n"
    inputs = [main, pathA, pathB, pathC, ack]
    input_idx, string_idx = 0,0
    def feed_input():
        nonlocal input_idx, string_idx
        if input_idx < len(inputs) and string_idx >= len(inputs[input_idx]):
            string_idx = 0
            input_idx += 1
        if input_idx >= len(inputs):
            print("sending input 0")
            return 0
        val = ord(inputs[input_idx][string_idx])
        string_idx += 1
        print(f"sending input {val} ({chr(val)})")
        return val
    
    def raw_output(val):
        if val < 128:
            print(chr(val), end="")
        else:
            print(val)
    inp[0] = 2
    execute(inp, feed_input, raw_output)


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
