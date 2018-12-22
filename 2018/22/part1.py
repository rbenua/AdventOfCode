#!/usr/bin/env python3

import sys

grid = []
tx = -1
ty = -1
depth = -1

def init(target, indepth):
    global tx, ty, depth
    tx = target[0]
    ty = target[1]
    depth = indepth

def indexlevel(x, y):
    if (x, y) == (0, 0) or (x, y) == (tx, ty):
        index = 0
    elif y == 0:
        index = x * 16807
    elif x == 0:
        index = y * 48271
    else:
        index = grid[y-1][x][1] * grid[y][x-1][1]
    level = (index + depth) % 20183
    return (index, level)

def fill():
    for y in range(ty + 1):
        grid.append([(0,0)] * (tx + 1))
        for x in range(tx + 1):
            grid[y][x] = indexlevel(x, y)

def printgrid():
    for row in grid:
        for cell in row:
            print('.=|'[cell[1] % 3], end="")
        print()
    print()


if __name__ == "__main__":
    init((int(sys.argv[1]), int(sys.argv[2])), int(sys.argv[3]))
    fill()
    printgrid()
    print(sum([cell[1] % 3 for row in grid for cell in row]))
