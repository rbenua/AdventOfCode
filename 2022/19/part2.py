#!/usr/bin/env python3

import sys
import re
from collections import deque
from collections import defaultdict
from multiprocessing import Process, Queue

f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

def nbrs(p, mx, my):
    x, y = p
    all = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    return [(x, y) for (x, y) in all if 0 <= x < mx and 0 <= y < my]

def mdist(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def padd(a, b):
    return (a[0] + b[0], a[1] + b[1])

def cnbrs(c):
    x, y, z = c
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

bot_types = ["ore", "clay", "obsidian", "geode"]

blueprints = []
for line in f:
    [idx, orecost, claycost, obsore, obsclay, geore, geobs] = nums(line)
    blueprint = (idx, {
        "ore": [("ore", orecost)],
        "clay": [("ore", claycost)],
        "obsidian": [("ore", obsore), ("clay", obsclay)],
        "geode": [("ore", geore), ("obsidian", geobs)]
    })
    blueprints.append(blueprint)

def key(t, mats, bots):
    return (t, tuple(sorted(mats.items())), tuple(sorted(bots.items())))

cache = {}
hits = 0
total = 0
def best(t, to_beat, costs, mats, bots):
    #upper bound: if you made a geode bot every cycle, you'd get the triangular number of geodes
    global cache, hits, total
    total += 1
    k = key(t, mats, bots)
    res = cache.get(k)
    if res is not None:
        hits += 1
        return res
  
    time_left = 31 - t
    ub = time_left * (time_left + 1) / 2
    if to_beat >= ub:
        cache[k] = (0, [])
        return (0, [])

    if t >= 31:
        #print(t, costs, mats, bots, ": 0")
        return (0, [])
    b = to_beat
    best_action = []
    for ty in bot_types:
        nextmats = {k:v + bots[k] for (k, v) in mats.items()}
        can_afford = True
        for (m, amt) in costs[ty]:
            if mats[m] >= amt:
                nextmats[m] -= amt
            else:
                can_afford = False
                break
        if not can_afford:
            continue
        nextbots = bots.copy()
        nextbots[ty] += 1
        nowscore = 0
        if ty == "geode":
            nowscore = 31 - t
        (score, future) = best(t + 1, max(0, b - nowscore), costs, nextmats, nextbots)
        score += nowscore
        if score > b:
            #print(t, costs, mats, bots, ":", b)
            b = score
            best_action = [(t, ty, nowscore)] + future
    nextmats = {k:v + bots[k] for (k, v) in mats.items()}
    (idle_score, idle_future) = best(t + 1, b, costs, nextmats, bots)
    if idle_score > b:
        #print(t, costs, mats, bots, ":", b)
        b = idle_score
        best_action = idle_future
    if b > to_beat:
        cache[k] = (b, best_action)
        return (b, best_action)
    else:
        return (0, [])
        
def mbest(q, idx, costs):
    start_mats = {"ore":0, "clay":0, "obsidian":0}
    start_bots = {"ore":1, "clay":0, "obsidian":0, "geode":0}
    (score, hist) = best(0, 0, costs, start_mats, start_bots)
    q.put((idx, score, hist, len(cache), hits / total))

if __name__ == "__main__":
    total = 0
    q = Queue()
    ps = []
    for (idx, costs) in blueprints[0:3]:
        p = Process(target=mbest, args=(q, idx, costs))
        p.start()
        ps.append(p)

    for _ in range(3):
        (idx, score, hist, clen, hrate) = q.get()
        print(idx, score, clen, hrate, hist)
        total *= score

    print(total)

    for p in ps:
        p.join()
