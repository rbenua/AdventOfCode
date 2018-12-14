#!/usr/bin/env python3

import sys
import re

state_pat = re.compile('initial state: ([.#]+)')
rule_pat = re.compile('([#.]+) => ([#.])')

rules = {}
state = []
offset = 0

def read_input(filename):
    global state
    with open(filename, 'r') as f:
        l = f.readlines()
    statematch = state_pat.search(l[0])
    state = list(statematch.group(1))

    for line in l[2:]:
        rulematch = rule_pat.search(line)
        rules[tuple(rulematch.group(1))] = rulematch.group(2)

def step():
    global state, offset
    newstate = []
    if rules[('.','.','.')+tuple(state[0:2])] == '#':
        newstate.append('#')
        offset += 1

    newstate.append(rules[('.','.')+tuple(state[0:3])])
    newstate.append(rules[('.',)+tuple(state[0:4])])

    for i in range(len(state) - 4):
        newstate.append(rules[tuple(state[i:i+5])])

    newstate.append(rules[tuple(state[-4:]) + ('.',)])
    newstate.append(rules[tuple(state[-3:]) + ('.','.')])
    if rules[tuple(state[-2:]) + ('.','.','.')] == '#':
        newstate.append('#')

    i = 0
    while newstate[i] == '.':
        offset -= 1
        i += 1
    del newstate[:i]

    state = newstate

def value():
    v = 0
    for i in range(len(state)):
        if state[i] == '#':
            v += i - offset
    return v

def printrules():
    print('\n'.join([''.join(k) + " => " + v for (k, v) in rules.items()]))

def printstate():
    global state
    print(''.join(state))

if __name__ == "__main__":
    read_input(sys.argv[1])
    print(f' 0 {value():5} {offset:3} {"".join(state)}')
    for i in range(1,501):
        step()
        print(f'{i:2} {value():5} {offset:3} {"".join(state)}')
