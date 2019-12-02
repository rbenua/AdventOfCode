#!/usr/bin/env python3

import sys, re


class Square:
    Wall = 0
    Flowing = 1
    Still = 2
    def __init__(self, x, y, type):
        self.x = x
        self.y = y
        self.type = type
        self.blockLeft = False
        self.blockRight = False

    def __str__(self):
        if self.type == Square.Wall:
            return '#'
        if self.type == Square.Still:
            return '~'
        if self.type == Square.Flowing and self.blockLeft and self.blockRight:
            return '!' # this shouldn't happen!
        if self.type == Square.Flowing and self.blockLeft:
            return '\\'
        if self.type == Square.Flowing and self.blockRight:
            return '/'
        if self.type == Square.Flowing:
            return '|'
        return '?'

    def __repr__(self):
        return self.__str__()

yspat = re.compile("x=(\d+), y=(\d+)\.\.(\d+)")
xspat = re.compile("y=(\d+), x=(\d+)\.\.(\d+)")

miny = 999999
maxy = 0
minx = 999999
maxx = 0

grid = {(500,0): Square(500, 0, Square.Flowing)}
toProcess = {(500,0)}

def read_input(filename):
    global miny, maxy, minx, maxx
    with open(filename, 'r') as f:
        for line in f:
            match = yspat.search(line)
            if match:
                x = int(match.group(1))
                minx = min(minx, x)
                maxx = max(maxx, x)
                for y in range(int(match.group(2)), int(match.group(3)) + 1):
                    grid[(x,y)] = Square(x, y, Square.Wall)
                    miny = min(miny, y)
                    maxy = max(maxy, y)

            match = xspat.search(line)
            if match:
                y = int(match.group(1))
                miny = min(miny, y)
                maxy = max(maxy, y)
                for x in range(int(match.group(2)), int(match.group(3)) + 1):
                    grid[(x,y)] = Square(x, y, Square.Wall)
                    minx = min(minx, x)
                    maxx = max(maxx, x)

def printgrid():
    for y in range(miny, maxy + 1):
        for x in range(minx, maxx + 1):
            if (x,y) in grid:
                pass
                print(grid[(x,y)], end='')
            else:
                pass
                print('.', end='')
        print()

def quiesce(leftx, rightx, y):
    # Turn a boxed-in row of flowing water into standing.
    # Remove any unprocessed squares in this row from the frontier (if 2 streams fell into it at the same tick).
    # Add any flowing squares above this back to the frontier so more flow can spread on top of this.
    for x in range(leftx, rightx):
        sq = grid[(x,y)]
        if sq.type != Square.Flowing:
            raise ValueError
        sq.type = Square.Still
        toProcess.discard((x,y))
        if (x, y - 1) in grid and grid[(x,y-1)].type == Square.Flowing:
            toProcess.add((x,y-1))

def spread(sq):
    if sq.type != Square.Flowing:
        # We shouldn't try to spread from an already-quiesced square.
        raise ValueError
    # first, flow downward if possible
    if (sq.x, sq.y+1) not in grid:
        if sq.y < maxy:
            grid[(sq.x, sq.y+1)] = Square(sq.x, sq.y + 1, Square.Flowing)
            toProcess.add((sq.x,sq.y+1))
        return
    if grid[(sq.x,sq.y+1)].type == Square.Flowing:
        # We've fallen onto an existing sideways flow. No need to do anything more.
        return

    # Spread sideways in both directions until hitting a wall or nothing under us. 
    # We should not hit any still water but may hit other flowing water. Skip over it and continue.
    blocked_left = False
    leftx = 0
    rightx = 0
    x = sq.x
    y = sq.y
    while True:
        x -= 1
        if (x, y) not in grid:
            grid[(x,y)] = Square(x, y, Square.Flowing)
        elif grid[(x,y)].type == Square.Wall:
            blocked_left = True
            leftx = x + 1
            break
        if (x, y + 1) not in grid or grid[(x,y+1)].type == Square.Flowing:
            # We've run off an edge. Add the last horizontal flow to the frontier and stop.
            toProcess.add((x,y))
            break

    blocked_right = False
    x = sq.x
    y = sq.y
    while True:
        x += 1
        if (x, y) not in grid:
            grid[(x,y)] = Square(x, y, Square.Flowing)
        if grid[(x,y)].type == Square.Wall:
            blocked_right = True
            rightx = x
            break
        if (x, y + 1) not in grid or grid[(x,y+1)].type == Square.Flowing:
            # We've run off an edge. Add the last horizontal flow to the frontier and stop.
            toProcess.add((x,y))
            break

    # If we ran into walls on both sides, turn the whole lot into still water
    if blocked_left and blocked_right:
        quiesce(leftx, rightx, y)

def simulate():
    while len(toProcess) != 0:
        printgrid()
        print()
        pos = toProcess.pop()
        spread(grid[pos])


def run(filename):
    read_input(filename)
    #printgrid()
    simulate()
    #print()
    printgrid()
    print(len([x for ((x,y),sq) in grid.items() if y >= miny and sq.type == Square.Still]))


if __name__ == '__main__':
    run(sys.argv[1])
