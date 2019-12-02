#!/usr/bin/env python3

import sys, re, collections

#pat = re.compile("(\w+) (\w)( [a-z]|[\d-]+)?")

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(l) for l in f.read().split(",")]

    
def execute(inp):
    pc = 0
    while True:
        if pc < 0 or pc >= len(inp) or inp[pc] == 99:
            return
        elif inp[pc] == 1:
            inp[inp[pc+3]] = inp[inp[pc+1]] + inp[inp[pc+2]]
            pc += 4
        elif inp[pc] == 2:
            inp[inp[pc+3]] = inp[inp[pc+1]] * inp[inp[pc+2]]
            pc += 4
        else:
            print("illegal opcode!" + inp[pc])
            return
            
        
def part1(inp):
    inp[1] = 12
    inp[2] = 2
    execute(inp)
    return inp[0]
    
    
    

def part2(inp):
    for v1 in range(100):
        print(v1)
        for v2 in range(100):
            l = inp.copy()
            l[1] = v1
            l[2] = v2
            execute(l)
            if l[0] == 19690720:
                return 100 * v1 + v2


def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    inp = read_input(filename)
    print(part2(inp))


if __name__ == "__main__":
    run(sys.argv[1])
