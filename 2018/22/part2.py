#!/usr/bin/env python3

import sys

grid = []
tx = -1
ty = -1
depth = -1

TORCH = 0
ROPE = 1
NEITHER = 2

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

def extendx():
    for y in range(len(grid)):
        grid[y].append(indexlevel(len(grid[y]), y))

def extendy():
    grid.append([(0,0)] * len(grid[0]))
    y = len(grid) - 1
    for x in range(len(grid[y])):
        grid[y][x] = indexlevel(x, y)

def legal(pos):
    ((curx, cury), curtool) = pos
    if curx < 0 or cury < 0:
        return False
    roomtype = grid[cury][curx][1] % 3
    if roomtype == 0 and curtool == NEITHER:
        return False
    if roomtype == 1 and curtool == TORCH:
        return False
    if roomtype == 2 and curtool == ROPE:
        return False
    return True

def neighbor_weights(pos):
    ((curx, cury), curtool) = pos
    adjs = [(((curx, cury), (curtool + 1) % 3), 7), (((curx, cury), (curtool + 2) % 3), 7),
            (((curx + 1, cury), curtool), 1), (((curx - 1, cury), curtool), 1),
            (((curx, cury + 1), curtool), 1), (((curx, cury - 1), curtool), 1)]
    if curx == len(grid[0]) - 1:
        extendx()
    if cury == len(grid) - 1:
        extendy()
    return [(p, d) for (p, d) in adjs if legal(p)]

def shortest_path(source, target):
    Q = {source: 0}
    visited = {source}
    prevs = {}
    seen = 0
    while len(Q) > 0:
        (currdist, currpos) = min([(d, p) for (p, d) in Q.items()])
        #print("Shortest distance to {0[0]}, {1} is {2}, from {3}".format(currpos, ('TORCH','ROPE','NONE')[currpos[1]], currdist, prevs.get(currpos, 0)))
        if seen % 1000 == 0:
            print("explored {} points, out to distance {}".format(seen, currdist))
        seen += 1
        if currpos == target:
            dist = currdist
            path = [(currpos, currdist)]
            while currpos in prevs:
                (currpos, currdist) = prevs[currpos]
                path.append((currpos, currdist))
            return (path, dist)

        del Q[currpos]
        visited.add(currpos)
        for (n, d) in neighbor_weights(currpos):
            if n not in visited and Q.get(n, 999999) > currdist + d:
                Q[n] = currdist + d
                prevs[n] = (currpos, currdist)


def printgrid(path=None):
    if path:
        path = [p for ((p, tool), dist) in path]
    y = 0
    for row in grid:
        x = 0
        for cell in row:
            if x == tx and y == ty:
                print('T', end="")
            elif path and (x, y) in path:
                print('X', end="")
            else:
                print('.=|'[cell[1] % 3], end="")
            x += 1
        print()
        y += 1
    print()


if __name__ == "__main__":
    init((int(sys.argv[1]), int(sys.argv[2])), int(sys.argv[3]))
    fill()
    print(sum([cell[1] % 3 for row in grid for cell in row]))
    (path, dist) = shortest_path(((0,0), TORCH), ((tx, ty), TORCH))
    print(dist)
    #printgrid(path)
