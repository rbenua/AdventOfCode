#!/usr/bin/env python3

import sys, re, collections, math

pat = re.compile("x=(-?\d+), y=(-?\d+), z=(-?\d+)")
def read_input(filename):
    with open(filename, 'r') as f:
        arr = []
        for line in f:
            match = pat.search(line)
            p = [int(match.group(1)), int(match.group(2)), int(match.group(3))]
            arr.append(p)
        return arr

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
            
        
def pairs(l):
    ps = []
    for i in range(l):
        ps += [(i, j) for j in range(i+1, l)]
    return ps

def energy(ps, vs):
    return sum(sum(abs(c) for c in p) * sum(abs(c) for c in v) for (p, v) in zip(ps, vs))

def tuplify(l):
    return tuple(tuple(c) for c in l)

def part1(inp):
    ps = [moon.copy() for moon in inp]
    vs = [[0, 0, 0] for _ in ps]
    for step in range(1000):
        for (a, b) in pairs(len(ps)):
            for coord in range(3):
                if ps[a][coord] < ps[b][coord]:
                    vs[a][coord] += 1
                    vs[b][coord] -= 1
                elif ps[a][coord] > ps[b][coord]:
                    vs[a][coord] -= 1
                    vs[b][coord] += 1
        for i in range(len(ps)):
            for c in range(len(ps[i])):
                ps[i][c] += vs[i][c]
    return energy(ps, vs)

def lcm(a, b):
    return (a * b) // math.gcd(a, b)

def part2(inp):
    periods = [0, 0, 0]
    for axis in range(3):
        ps = [moon[axis] for moon in inp]
        vs = [0 for _ in ps]
        seen = set()
        t = (tuple(ps), tuple(vs))
        while t not in seen:
            seen.add(t)
            for (a, b) in pairs(len(ps)):
                if ps[a] < ps[b]:
                    vs[a] += 1
                    vs[b] -= 1
                elif ps[a] > ps[b]:
                    vs[a] -= 1
                    vs[b] += 1
            for i in range(len(ps)):
                ps[i] += vs[i]
            t = (tuple(ps), tuple(vs))
        periods[axis] = len(seen)
        print(periods[axis])
    return lcm(lcm(periods[0], periods[1]), periods[2])



        


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
