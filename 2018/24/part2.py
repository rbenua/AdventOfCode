#!/usr/bin/env python3

import sys, re

class Group:
    def __init__(self, id, num, hp, atkpwr, atktype, weak, immune, init, infect):
        self.id = id
        self.num = num
        self.hp = hp
        self.atkpwr = atkpwr
        self.atktype = atktype
        self.weak = weak
        self.init = init
        self.immune = immune
        self.infect = infect

    def __str__(self):
        return "{} group {}: {} units {} hp {} atk ({})".format(("immune","infect")[self.infect], self.id, self.num, self.hp, self.atkpwr, self.atktype)
    def __repr__(self):
        return self.__str__()

allgroups = []

pat = re.compile("(\d+) units each with (\d+) hit points( \(.+\))? with an attack that does (\d+) ([a-z]+) damage at initiative (\d+)")
weakpat = re.compile("weak to (([a-z]+[, ]*)+)")
immunepat = re.compile("immune to (([a-z]+[, ]*)+)")

def init(filename, boost):
    global allgroups, immunegroups, infectgroups
    allgroups = []
    immunegroups = []
    infectgroups = []
    id = 1
    with open(filename, 'r') as f:
        infect = False
        for line in f:
            if(line == "Infection:\n"):
                infect = True
                id = 1
                continue
            match = pat.search(line)
            if not match:
                continue
            num = int(match.group(1))
            hp = int(match.group(2))
            pstring = match.group(3)
            atkpwr = int(match.group(4)) + (boost,0)[infect]
            atktype = match.group(5)
            init = int(match.group(6))
            weak = []
            immune = []
            if pstring:
                mw = weakpat.search(pstring)
                if mw:
                    weak = mw.group(1).split(', ')
                mi = immunepat.search(pstring)
                if mi:
                    immune = mi.group(1).split(', ')
            g = Group(id, num, hp, atkpwr, atktype, weak, immune, init, infect)
            allgroups.append(g)
            id += 1
            
def damage(source, target):
    if source.atktype in target.immune:
        return 0
    d = source.num * source.atkpwr
    if source.atktype in target.weak:
        d *= 2
    return d

def find_target(source, avail):
    target = max([(damage(source, t), t.num * t.atkpwr, t.init, t) for t in avail if t.infect != source.infect] + [(0,)])
    if(target[0] > 0):
        avail.remove(target[3])
        return target[3]
    return None

def target_select():
    allgroups.sort(key=lambda g: g.atkpwr * g.num, reverse=True)
    targets = []
    avail = set(allgroups)
    for g in allgroups:
        t = find_target(g, avail)
        if t:
            targets.append((g, t))
    return targets

def attack(source, target):
    power = source.num * source.atkpwr
    if source.atktype in target.weak:
        power *= 2
    elif source.atktype in target.immune:
        power = 0
    dead = int(power / target.hp)
    target.num -= dead
    print("{} {} attacks {} for {}, kills {}, {} left".format(("immune", "infection")[source.infect], source.id, target.id, power, dead, target.num))
    if target.num <= 0:
        allgroups.remove(target)
        return True
    return False

def attack_phase(targets):
    targets.sort(key=lambda gs: gs[0].init, reverse=True)
    dead = set()
    for (g, t) in targets:
        if g in dead or t in dead:
            continue
        if attack(g, t):
            dead.add(t)

def winner():
    i = allgroups[0].infect
    for g in allgroups:
        if g.infect != i:
            return False
    return True

def simulate():
    i = 0
    while not winner() and i < 10000:
        print('round', i)
        attack_phase(target_select())
        i += 1

def run(filename):
    init(filename, 0)
    boost = 10
    while max([g.infect for g in allgroups]):
        print("trying", boost)
        init(filename, boost)
        simulate()
        print(allgroups)
        boost += 10
    boost -= 19
    init(filename, boost)
    while max([g.infect for g in allgroups]):
        print("trying", boost)
        init(filename, boost)
        simulate()
        print(allgroups)
        boost += 1
    boost -= 1
    print(boost)
    print(sum([g.num for g in allgroups]))


if __name__ == "__main__":
    run(sys.argv[1])
