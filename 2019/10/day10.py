#!/usr/bin/env python3

import sys, re, collections, itertools, math

def read_input(filename):
    with open(filename, 'r') as f:
        s = set()
        ls = f.readlines()
        for y in range(len(ls)):
            line = ls[y].strip()
            s.update((x, y) for x in range(len(line)) if line[x] == '#')
        return (s, len(ls), len(ls[0].strip()))
            
def visible_set(inp, currx, curry):
    (m, height, width) = inp
    cands = m - {(currx, curry)}
    seen = set()
    todo = cands - seen
    while len(todo) > 0:
        (candx, candy) = todo.pop()
        seen |= {(candx, candy)}
        dx = candx - currx
        dy = candy - curry
        g = math.gcd(dx, dy)
        dx = int(dx / g)
        dy = int(dy / g)
        
        x = candx 
        y = candy
        while 0 <= x < width and 0 <= y < height:
            x += dx
            y += dy
            cands -= {(x, y)}
        todo = cands - seen
    return cands

def part1(inp):
    (m, height, width) = inp
    best = (0, (0,0), set())
    for (currx, curry) in m:
        cands = visible_set(inp, currx, curry)
        best = max(best, (len(cands), (currx, curry), cands))
    print_grid((best[2], height, width), *best[1])
    return best

def print_grid(inp, fx = -1, fy = -1):
    (m, height, width) = inp
    for y in range(height):
        print("".join([".#*"[((x, y) in m) + ((x, y) == (fx, fy))] for x in range(width)]))

def angle(x, y):
    return math.atan2(-x - 0.001, y)

def part2(inp, res1, goal = 200):
    (m, height, width) = inp
    removed = 0
    while len(m) > 1:
        to_remove = visible_set(inp, *res1[1])
        m -= to_remove
        if (removed + len(to_remove)) >= goal:
            idx = goal - removed
            ls = sorted([(angle(x - res1[1][0], y - res1[1][1]), x, y) for (x, y) in to_remove])
            print(ls)
            nth = ls[idx-1]
            return nth[1] * 100 + nth[2]
        removed += len(to_remove)
        print(f"After first {removed} removed:")
        print_grid((m, height, width), *res1[1])

def run(filename):
    inp = read_input(filename)
    res1 = part1(inp)
    print(res1)
    print(part2(inp, res1))

if __name__ == "__main__":
    run(sys.argv[1])
