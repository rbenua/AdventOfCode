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

def rgnpower(x, y):
    return sum(grid[x][y:y+3]) + sum(grid[x+1][y:y+3]) + sum(grid[x+2][y:y+3])

print(max([(rgnpower(x,y), x + 1, y + 1) for x in range(297) for y in range(297)]))

