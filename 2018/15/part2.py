#!/usr/bin/env python3

import sys, enum

# get all neighbors of p in reading order
def nbrs(p):
    (y,x) = p
    return ((y-1,x),(y,x-1),(y,x+1),(y+1,x))

def mdist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

class LocType(enum.Enum):
    empty = 0
    wall = 1
    elf = 2
    goblin = 3

class Loc:
    def __init__(self, t, unit = None):
        self.t = t
        self.unit = unit

class Unit:
    def __init__(self, pos, is_goblin, atkpower):
        self.hp = 200
        self.pos = pos
        self.is_goblin = is_goblin
        self.attack_power = atkpower

grid = []
elves = []
goblins = []
allUnits = []

def read_input(filename, elfpower):
    with open(filename, 'r') as f:
        (y, x) = (0,0)
        for line in f:
            l = []
            x = 0
            for c in line.strip():
                if c == '#':
                    l.append(Loc(LocType.wall))
                elif c == '.':
                    l.append(Loc(LocType.empty))
                elif c == 'E':
                    e = Unit((y,x,),False, elfpower)
                    elves.append(e)
                    allUnits.append(e)
                    l.append(Loc(LocType.elf, e))
                elif c == 'G':
                    g = Unit((y,x,),True, 3)
                    goblins.append(g)
                    allUnits.append(g)
                    l.append(Loc(LocType.goblin, g))
                elif c == ' ':
                    break
                else:
                    raise ValueError
                x += 1
            grid.append(l)
            y += 1

def clear_state():
    global grid, elves, goblins, allUnits
    grid = []
    elves = []
    goblins = []
    allUnits = []

def find_enemy_adj(is_goblin):
    enemies = elves if is_goblin else goblins
    return {n for e in enemies for n in nbrs(e.pos)}

# Bastardized Dijkstra's Algorithm
def find_paths(start, targets):
    prevs = {}
    Q = {start:0}
    visited = {start}
    while len(Q) > 0:
        (currdist, currpos) = min([(d, p) for (p,d) in Q.items()])
        if currpos in targets:
            while prevs[currpos] != start:
                currpos = prevs[currpos]
            return currpos

        del Q[currpos]
        visited.add(currpos)
        for n in nbrs(currpos):
            if n not in visited and Q.get(n, 999999) > currdist + 1 and grid[n[0]][n[1]].t == LocType.empty:
                Q[n] = currdist + 1
                prevs[n] = currpos
    return None

def move_phase(u):
    targets = find_enemy_adj(u.is_goblin)
    if u.pos in targets:
        return
    dest = find_paths(u.pos, targets)
    if not dest:
        return
    grid[u.pos[0]][u.pos[1]].t = LocType.empty
    grid[u.pos[0]][u.pos[1]].unit = None
    u.pos = dest
    grid[u.pos[0]][u.pos[1]].t = (LocType.elf, LocType.goblin)[u.is_goblin]
    grid[u.pos[0]][u.pos[1]].unit = u

def kill(u):
    #print(('Elf','Goblin')[u.is_goblin],'at', u.pos, 'dies')
    allUnits.remove(u)
    if u.is_goblin:
        goblins.remove(u)
    else:
        elves.remove(u)
    grid[u.pos[0]][u.pos[1]].t = LocType.empty
    grid[u.pos[0]][u.pos[1]].unit = None

def attack_phase(u):
    targets = [] 
    for target in nbrs(u.pos):
        if grid[target[0]][target[1]].t == (LocType.goblin,LocType.elf)[u.is_goblin]:
            tu = grid[target[0]][target[1]].unit
            targets.append((tu.hp, tu.pos, tu))
    if len(targets) == 0:
        return
    _, _, tu = min(targets)
    #print(('Elf','Goblin')[u.is_goblin],'at', u.pos, 'attacks',('elf','goblin')[tu.is_goblin], 'at', tu.pos, 'with', tu.hp, 'HP')
    tu.hp -= u.attack_power
    if tu.hp < 0:
        kill(tu)
    return

def printgrid():
    for line in grid:
        units = []
        for square in line:
            print(('.','#','E','G')[square.t.value], end='')
            if square.unit:
                units.append(square.unit)
        for u in units:
            print(' {}({})'.format(('E','G')[u.is_goblin], u.hp), end='')
        print()

debug = False
def simulate(mrounds=99999):
    rounds = 0
    while rounds < mrounds:
        if debug:
            print([(u.pos, u.is_goblin) for u in allUnits])
        for unit in sorted(allUnits, key=lambda u: u.pos):
            if grid[unit.pos[0]][unit.pos[1]].unit != unit:
                # this unit died earlier in the round
                continue
            if debug:
                print("starting", ('Elf','Goblin')[unit.is_goblin], "at", unit.pos)
                print("e:",[e.pos for e in elves])
                print("g:",[g.pos for g in goblins])
            if (unit.is_goblin and len(elves) == 0) or (not unit.is_goblin and len(goblins) == 0):
                print(unit.pos)
                return rounds
            move_phase(unit)
            attack_phase(unit)
        #print("Round", rounds)
        #printgrid()
        rounds += 1
    return rounds

def run(filename):
    elfpower = 3
    while True:
        clear_state()
        read_input(filename, elfpower)
        num_elves = len(elves)
        rounds = simulate()
        if len(allUnits) == num_elves and not allUnits[0].is_goblin:
            break
        elfpower += 10

    elfpower -= 9
    while True:
        clear_state()
        read_input(filename, elfpower)
        num_elves = len(elves)
        rounds = simulate()
        if len(allUnits) == num_elves and not allUnits[0].is_goblin:
            break
        elfpower += 1
    
    hitpoints = sum([u.hp for u in allUnits])
    print()
    printgrid()
    print(rounds * hitpoints, rounds, hitpoints)

if __name__ == "__main__":
    run(sys.argv[1])
