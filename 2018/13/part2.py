#!/usr/bin/env python3

import sys
import enum

class Track(enum.Enum):
    none = 0
    up_down = enum.auto()
    left_right = enum.auto()
    up_left = enum.auto()
    down_right = enum.auto()
    down_left = enum.auto()
    up_right = enum.auto()
    cross = enum.auto()

class CartDir(enum.Enum):
    up = 0
    right = 1
    down = 2
    left = 3

map = []
carts = {}

def read_input(filename):
    with open(filename, 'r') as f:
        y = 0
        for line in f:
            l = []
            for x in range(len(line)):
                if line[x] == ' ' or line[x] == '\n':
                    l.append(Track.none)
                elif line[x] == '|':
                    l.append(Track.up_down)
                elif line[x] == '-':
                    l.append(Track.left_right)
                elif line[x] == '/':
                    if line[x+1] in '-<>+':
                        l.append(Track.down_right)
                    else:
                        l.append(Track.up_left)
                elif line[x] == '\\':
                    if line[x+1] in '-<>+':
                        l.append(Track.up_right)
                    else:
                        l.append(Track.down_left)
                elif line[x] == '+':
                    l.append(Track.cross)
                elif line[x] == '^':
                    l.append(Track.up_down)
                    carts[(y,x)] = (CartDir.up, 0)
                elif line[x] == 'v':
                    l.append(Track.up_down)
                    carts[(y,x)] = (CartDir.down, 0)
                elif line[x] == '>':
                    l.append(Track.left_right)
                    carts[(y,x)] = (CartDir.right, 0)
                elif line[x] == '<':
                    l.append(Track.left_right)
                    carts[(y,x)] = (CartDir.left, 0)
            y += 1
            map.append(l)

def step():
    for ((y,x),(d,i)) in sorted(carts.items()):
        if (y,x) not in carts:
            # we removed this one in an earlier collision
            continue
        if d == CartDir.up:
            newpos = (y-1,x)
        elif d == CartDir.down:
            newpos = (y+1,x)
        elif d == CartDir.left:
            newpos = (y,x-1)
        else:
            newpos = (y,x+1)
        if newpos in carts:
            del carts[(y,x)]
            del carts[newpos]
            continue
        
        dt = map[newpos[0]][newpos[1]]
        newi = i
        if dt == Track.up_down or dt == Track.left_right:
            newdir = d
        elif (dt == Track.up_left and d == CartDir.down) or (dt == Track.down_left and d == CartDir.up):
            newdir = CartDir.left
        elif (dt == Track.up_right and d == CartDir.down) or (dt == Track.down_right and d == CartDir.up):
            newdir = CartDir.right
        elif (dt == Track.up_left and d == CartDir.right) or (dt == Track.up_right and d == CartDir.left):
            newdir = CartDir.up
        elif (dt == Track.down_left and d == CartDir.right) or (dt == Track.down_right and d == CartDir.left):
            newdir = CartDir.down
        elif dt == Track.cross:
            if i == 0:
                newdir = CartDir((d.value - 1) % 4)
            elif i == 1:
                newdir = d
            elif i == 2:
                newdir = CartDir((d.value + 1) % 4)
            newi = (i + 1) % 3
        else:
            print("Derailed",(y,x),"=>",newpos)
            raise Error()

        del carts[(y,x)]
        carts[newpos] = (newdir, newi)

    if len(carts) == 1:
        return carts.items()
    return None

if __name__ == "__main__":
    read_input(sys.argv[1])
    while True:
        p = step()
        if p is not None:
            print(p)
            exit(0)
