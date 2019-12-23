#!/usr/bin/env python3

import sys, re, collections, math, heapq

DBG = 0
def debug(*args, **kwargs):
    if DBG:
        print(*args, **kwargs)

def read_input(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
        tags = {}
        locs = collections.defaultdict(lambda:[None, None])
        maxx = len(lines[0]) - 1
        maxy = len(lines)
        for (y, line) in enumerate(lines):
            for (x, c) in enumerate(line):
                if c == '.':
                    for (dir, (x2, y2)) in neighbors((x, y)):
                        if lines[y2][x2].isalpha():
                            tag = ""
                            if dir == 1:
                                tag = lines[y2-1][x2] + lines[y2][x2]
                            elif dir == 2:
                                tag = lines[y2][x2] + lines[y2+1][x2]
                            elif dir == 3:
                                tag = lines[y2][x2-1] + lines[y2][x2]
                            elif dir == 4:
                                tag  = lines[y2][x2] + lines[y2][x2+1]
                            outer = x == 2 or y == 2 or x == maxx - 3 or y == maxy - 3
                            tags[(x, y)] = (tag, outer)
                            locs[tag][outer] = (x, y)

        debug("tags:", tags)
        debug("locs:", locs)
        dists = collections.defaultdict(list)
        for (start, (tag, outer)) in tags.items():
            seen = set()
            todo = [(0, start)]
            while len(todo) > 0:
                (dist, here) = todo.pop(0)
                seen.add(here)
                if here in tags and here != start:
                    dists[(tag, outer)].append((dist, tags[here]))
                for (_, nbr) in neighbors(here):
                    (x2, y2) = nbr
                    if nbr not in seen and lines[y2][x2] == '.':
                        todo.append((dist + 1, nbr))

        debug("dists:", dists)
        return (tags, locs, dists)
            
def neighbors(here):
    x, y = here
    return ((1, (x, y-1)), (2, (x, y+1)), (3, (x-1, y)), (4, (x+1, y)))

def part1(inp):
    (tags, locs, dists) = inp
    todo = [(0, ("AA", True))]
    seen = {("AA", False)}
    while len(todo) > 0:
        (dist, here) = heapq.heappop(todo)
        debug(dist, here)
        if here == ("ZZ", True):
            return dist
        seen.add(here)
        tag, outer = here
        if (tag, not outer) not in seen:
            heapq.heappush(todo, (dist + 1, (tag, not outer)))
        for (delta, nbr) in dists[here]:
            if nbr not in seen:
                heapq.heappush(todo, (dist + delta, nbr))


def part2(inp):
    (tags, locs, dists) = inp
    todo = [(0, ("AA", True), 0)]
    seen = set()
    while len(todo) > 0:
        (dist, here, layer) = heapq.heappop(todo)
        debug(dist, here, layer)
        if here == ("ZZ", True) and layer == 0:
            return dist
        seen.add((here, layer))
        tag, outer = here
        newlayer = (layer+1, layer-1)[outer]
        if newlayer >= 0 and tag != "AA" and ((tag, not outer), newlayer) not in seen:
            heapq.heappush(todo, (dist + 1, (tag, not outer), newlayer))
        for (delta, nbr) in dists[here]:
            if (nbr, layer) not in seen and nbr[0] != "AA":
                heapq.heappush(todo, (dist + delta, nbr, layer))
        
def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
