#!/usr/bin/env python3

import sys, re

empty = 0
trees = 1
yard = 2

def read_input(filename):
    grid = []
    with open(filename, 'r') as f:
        for line in f:
            l = []
            for c in line.strip():
                if c == '.':
                    l.append(empty)
                elif c == '|':
                    l.append(trees)
                elif c == '#':
                    l.append(yard)
                else:
                    raise ValueError
            grid.append(l)
    return grid

def nbrs(x, y, nx, ny):
    n = [(x-1,y-1),(x,y-1),(x+1,y-1),
         (x-1,y),          (x+1,y),
         (x-1,y+1),(x,y+1),(x+1,y+1)]
    return [(x,y) for (x,y) in n if x >= 0 and y >= 0 and x < nx and y < ny]

def count(l, t):
    c = 0
    for i in l:
        if t == i:
            c += 1
    return c

def step(grid, dbgx, dbgy):
    newgrid = []
    nx = len(grid[0])
    ny = len(grid)
    for y in range(nx):
        newline = []
        for x in range(ny):
            dbg = x == dbgx and y == dbgy
            ns = [grid[y][x] for (x,y) in nbrs(x, y, nx, ny)]
            newval = grid[y][x]
            if dbg:
                print("debug at", x, y)
                print("initial value", newval, "neighbor coords", nbrs(x,y,nx,ny),"neighbors", ns)
                print(count(ns, trees), "trees,", count(ns,yard), "yards")
            if grid[y][x] == empty and count(ns, trees) >= 3:
                if dbg:
                    print("new tree")
                newval = trees
            if grid[y][x] == trees and count(ns, yard) >= 3:
                if dbg:
                    print("new yard")
                newval = yard
            if grid[y][x] == yard and (trees not in ns or yard not in ns):
                if dbg:
                    print("yard decays")
                newval = empty
            newline.append(newval)
        newgrid.append(newline)
    return newgrid

def printgrid(grid):
    for line in grid:
        for s in line:
            if s is empty:
                print('.', end='')
            elif s is trees:
                print('|', end='')
            elif s is yard:
                print('#', end='')
            else:
                raise ValueError
        print()

def run(filename, dbgx=-1, dbgy=-1):
    grid = read_input(filename)
    printgrid(grid)
    print()
    for _ in range(10):
        grid = step(grid, dbgx, dbgy)
        printgrid(grid)
        print()
    
    numyards = len([1 for l in grid for c in l if c is yard])
    numtrees = len([1 for l in grid for c in l if c is trees])
    print(numyards, numtrees, numyards * numtrees)

if __name__ == "__main__":
    run(sys.argv[1])

