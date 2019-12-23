#!/usr/bin/env python3

import sys, re, collections, math, threading, queue

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
    while True:
        yield val

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

def run_thread(inp, addr, queues):
    def in_receive(myaddr):
        yield myaddr
        while True:
            try:
                (x, y) = queues[myaddr].get(False)
                yield x
                yield y
            except queue.Empty:
                yield -1
    out_buf = []    
    def out_send(val):
        nonlocal out_buf
        out_buf.append(val)
        if len(out_buf) == 3:
            if out_buf[0] == 255:
                out_buf[0] = 50
            queues[out_buf[0]].put((out_buf[1], out_buf[2]))
            out_buf = []
    
    execute(inp.copy(), in_receive(addr), out_send)

def part1(inp):
    queues = [queue.Queue() for _ in range(51)]
    for addr in range(50):
        t = threading.Thread(target=run_thread, args=(inp, addr, queues))
        t.daemon = True
        t.start()
    (x, y) = queues[50].get()
    return y #this is going to leave all these threads around until the process exits, 
             #but i don't want to try and write a check for  a flag into the middle of 
             #the interpreter loop and python doesn't let you kill other threads in a GIL-safe way :|

def part2(inp):
    pass

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
