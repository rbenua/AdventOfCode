#!/usr/bin/env python3

import sys

pattern = []
adjs = {(0,0): set()}

def read_input(filename):
    global pattern
    with open(filename, 'r') as f:
        pattern = list(f.read().strip('\n^$'))

def add_edge(curr_pos, dir):
    (y, x) = curr_pos
    if dir == 'N':
        dest = (y - 1, x)
    elif dir == 'E':
        dest = (y, x + 1)
    elif dir == 'S':
        dest = (y + 1, x)
    elif dir == 'W':
        dest = (y, x - 1)
    else:
        raise ValueError

    adjs[curr_pos].add(dest)
    adjs.setdefault(dest, set())
    adjs[dest].add(curr_pos)
    return dest

def build_adjs(idx, currs):
    starts = currs.copy()
    ends = set()
    while idx < len(pattern):
        if pattern[idx] in 'NESW':
            currs = {add_edge(curr_pos, pattern[idx]) for curr_pos in currs}
            idx += 1
        elif pattern[idx] == '(':
            (idx, currs) = build_adjs(idx + 1, currs)
        elif pattern[idx] == '|':
            idx += 1
            ends |= currs
            currs = starts.copy()
        elif pattern[idx] == ')':
            return (idx + 1, ends | currs)
        else:
            raise ValueError
    return idx

def shortest_paths(source):
    Q = {source: 0}
    visited = {source: 0}
    while len(Q) > 0:
        (currdist, currpos) = min([(d, p) for (p, d) in Q.items()])
        del Q[currpos]
        visited[currpos] = currdist
        for n in adjs[currpos]:
            if n not  in visited and Q.get(n, 999999) > currdist + 1:
                Q[n] = currdist + 1

    return visited

def run(filename):
    read_input(filename)
    build_adjs(0, {(0,0)})
    p = shortest_paths((0,0))
    print(p)
    print(max(p.values()))

if __name__ == "__main__":
    run(sys.argv[1])
