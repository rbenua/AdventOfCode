#!/usr/bin/env python3

import sys

grid_num = int(sys.argv[1])

# NOTE: coordinates used in this code are 0-indexed, so add 1 for problem-defined coords

def power(x, y):
    rack_id = x + 11
    pl = (y + 1) * rack_id
    pl += grid_num
    pl *= rack_id
    pl = int((pl / 100) % 10) # these numbers are always positive
    return pl - 5

grid = [[power(x, y) for y in range(300)] for x in range(300)]



def rgnpower(x, y, size, memo):
    return memo[x][y] + sum(grid[x+size-1][y:y+size]) + sum([grid[x+i][y+size-1] for i in range(size-1)])

def quadpower(x, y, size):
    return sum([grid[lx][ly] for lx in range(x, x + size) for ly in range(y, y + size)])

def update(size, memo):
    lmax = (0,0,0)
    for x in range(301 - size):
        for y in range(301 - size):
            p = rgnpower(x, y, size, memo)
            #print((x,y,p))
            memo[x][y] = p
            lmax = max(lmax, (p, x+1, y+1))
    return lmax

def run():
    best = ((0,0,0),0)
    memo = [[0] * 300 for i in range(300)]
    for size in range(1, 301):
        lmax = update(size, memo)
        best = max(best, (lmax, size))
        print((lmax, size))
    print(best)

def runquad():
    pwrs = []
    for sz in range(1,301):
        lmax = max([(quadpower(x,y,sz),x+1,y+1,sz) for x in range(301-sz) for y in range(301-sz)])
        print(lmax)
        pwrs.append(lmax)
    print(max(pwrs))

if __name__ == '__main__':
    #runquad()
    run()
