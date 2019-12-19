#!/usr/bin/env python3

import sys, re, collections, math, bisect

def read_input(filename):
    with open(filename, 'r') as f:
        return f.readlines()
        
def neighbors(here):
    x, y = here
    return ((x, y-1), (x, y+1), (x-1, y), (x+1, y))

def part1(inp):
    locs = {}
    for (y, row) in enumerate(inp):
        for (x, c) in enumerate(row.strip()):
            if c != "#" and c != ".":
                locs[c] = (x, y)

    blocked = {}
    dists = {}
    to_explore = [(locs["@"], frozenset(), 0)]
    explored = set()
    while len(to_explore) > 0:
        (here, blocks, dist) = to_explore.pop()
        explored.add(here)
        for (x, y) in neighbors(here):
            c = inp[y][x]
            if c == '#' or (x, y) in explored:
                continue
            elif c == '.':
                to_explore.append(((x, y), blocks, dist + 1))
            elif c.islower():
                blocked[c] = blocks
                dists[("@", c)] = dist + 1
                to_explore.append(((x, y), blocks, dist + 1))
            else:
                to_explore.append(((x, y), blocks | {c.lower()}, dist + 1))

    for start in blocked.keys():
        to_explore = [(locs[start], 0)]
        explored = set()
        while len(to_explore) > 0:
            (here, dist) = to_explore.pop(0)
            explored.add(here)
            for (x, y) in neighbors(here):
                c = inp[y][x]
                if c == '#' or (x, y) in explored:
                    continue
                if c.islower():
                    dists[tuple(sorted([start, c]))] = dist + 1
                to_explore.append(((x, y), dist + 1))
    print(blocked)
    print(dists)
    num_keys = len(blocked.keys())
    all_keys = set(blocked.keys())

    to_explore = [(0, "@", frozenset())]
    visited = set()

    def insert_sorted(l, v):
        (dist, end, seen) = v
        for (i, (idist, iend, iseen)) in enumerate(l):
            if (end, seen) == (iend, iseen):
                return
            if dist < idist:
                l.insert(i, v)
                return
        l.append(v)

    while len(to_explore) > 0:
        (dist, end, seen) = to_explore.pop(0)
        if (end, seen) in visited:
            continue
        visited.add((end, seen))
        if dist % 100 == 0:
            print(dist, end, seen, len(to_explore))
        if len(seen) == num_keys:
            return dist
        for nextkey in all_keys - seen:
            if len(blocked[nextkey] - seen) != 0:
                continue
            nexttuple = (dist + dists[tuple(sorted([end, nextkey]))], nextkey, seen | {nextkey})
            insert_sorted(to_explore, nexttuple)


def part2(inp):
    locs = {}
    for (y, row) in enumerate(inp):
        for (x, c) in enumerate(row.strip()):
            if c != "#" and c != ".":
                locs[c] = (x, y)
    (cx, cy) = locs["@"]
    inp = list(map(list, inp))
    inp[cy-1][cx-1:cx+2] = "@#@"
    inp[cy][cx-1:cx+2] =   "###"
    inp[cy+1][cx-1:cx+2] = "@#@"
    atlocs = [(cx-1, cy-1), (cx-1, cy+1), (cx+1, cy-1), (cx+1, cy+1)]

    blocked = {}
    dists = collections.defaultdict(list)
    # get distances from each @ to its reachable keys, noting which doors block each key
    # This thing seems to be a tree, so the only way to reach each key is by opening that set of doors
    for start in atlocs:
        to_explore = [(start, frozenset(), 0)]
        explored = set()
        while len(to_explore) > 0:
            (here, blocks, dist) = to_explore.pop()
            explored.add(here)
            for (x, y) in neighbors(here):
                c = inp[y][x]
                if c == '#' or (x, y) in explored:
                    continue
                elif c == '.':
                    to_explore.append(((x, y), blocks, dist + 1))
                elif c.islower():
                    blocked[c] = blocks
                    dists[start].append((c, dist + 1))
                    to_explore.append(((x, y), blocks, dist + 1))
                else:
                    to_explore.append(((x, y), blocks | {c.lower()}, dist + 1))

    # Extend the dependency dict to cover transitive dependencies.
    for (k, bs) in blocked.items():
        all_deps = set()
        todo = set(bs)
        while len(todo) > 0:
            c = todo.pop()
            all_deps.add(c)
            todo |= blocked[c] - all_deps
        blocked[k] = all_deps
            
    # BFS from each key to determine the rest of the distances between them
    for start in blocked.keys():
        to_explore = [(locs[start], 0)]
        explored = set()
        while len(to_explore) > 0:
            (here, dist) = to_explore.pop(0)
            explored.add(here)
            for (x, y) in neighbors(here):
                c = inp[y][x]
                if c == '#' or (x, y) in explored:
                    continue
                if c.islower():
                    dists[start].append((c, dist + 1))
                to_explore.append(((x, y), dist + 1))

    for k, v in dists.items():
        print(k, v)
    print()
    for k, v in blocked.items():
        print(k, v)
    
    def neighbor_states(st):
        (dist, cur, seen) = st
        nbrs = []
        for c in cur:
            for (np, delta) in dists[c]:
                if np not in seen and len(blocked[np] - seen) == 0:
                    nbrs.append((dist + delta, (cur - {c}) | {np}, seen | {np}))
        return sorted(nbrs)

    
    def insert_sorted(l, v):
        (dist, end, seen) = v
        for (i, (idist, iend, iseen)) in enumerate(l):
            if dist < idist:
                print("insert ", v)
                l.insert(i, v)
                return
            if (end, seen) == (iend, iseen):
                print("discard", v)
                return
        print("append ", v)
        l.append(v)

    all_keys = set(blocked.keys())

    def revdeps(k, seen, in_quadrant):
        result = set(k) | seen
        #print("testing against", result, in_quadrant)
        for k2 in all_keys - in_quadrant:
            if len((blocked[k2] - result) & in_quadrant) == 0:
                #print("adding", k2, blocked[k2])
                result.add(k2)
        return frozenset(result)
        
    
    num_keys = len(blocked.keys())
    total_distance = 0
    for start in atlocs:
        in_quadrant = set(k2 for (k2, _) in dists[start])
        to_explore = [(0, start, frozenset(all_keys - in_quadrant))]
        visited = set()

        print(f"Starting quadrant {start}, visible {in_quadrant}")
        while len(to_explore) > 0:
            (dist, end, seen) = to_explore.pop(0)
            if (end, seen) in visited:
                continue
            visited.add((end, seen))
            #if dist % 100 == 0:
            print(dist, end, seen, len(to_explore))
            if len(seen) == num_keys:
                print("found quadrant", start, dist)
                total_distance += dist
                break
            for (nextkey, delta) in dists[end]:
                if nextkey in seen or len((blocked[nextkey] - seen) & in_quadrant) != 0:
                    print("discard", nextkey, seen, blocked[nextkey])
                    continue
                nexttuple = (dist + delta, nextkey, seen | {nextkey})
                insert_sorted(to_explore, nexttuple)

    return total_distance

def run(filename):
    inp = read_input(filename)
    #print(part1(inp))
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
