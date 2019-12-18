#!/usr/bin/env python3

import sys, re, collections, math

pat = re.compile("([\w ,]+) => (\d+) (\w+)")

def read_input(filename):
    with open(filename, 'r') as f:
        recipes = {}
        for line in f:
            match = pat.search(line)
            output = match.group(3)
            outqty = int(match.group(2))
            inputs = tuple((int(s.strip().split(" ")[0]), s.strip().split(" ")[1]) for s in match.group(1).split(","))
            recipes[output] = (outqty, inputs)
        return recipes

def part1(inp):
    ore_needed = 0
    qtys = collections.defaultdict(int)
    qtys["FUEL"] = -1 * int(sys.argv[2])
    while min(qtys.values()) < 0:
        for output, qty in qtys.items():
            if qty < 0:
                (prodqty, reqs) = inp[output]
                nrx = math.ceil(abs(qty) / prodqty)
                qtys[output] += prodqty * nrx
                for (amt, typ) in reqs:
                    if typ == "ORE":
                        ore_needed += amt * nrx
                    else:
                        qtys[typ] -= amt * nrx
                break
    #print(qtys)
    return ore_needed

def part2(inp):
    return # i threw this out and binary searched by hand after optimizing part1
    qtys = collections.defaultdict(int)
    ore_needed = 0
    fuel_produced = 1
    qtys["FUEL"] = -1
    while True:
        if min(qtys.values()) >= 0:
            qtys["FUEL"] = -1
            fuel_produced += 1
            if fuel_produced % 10000 == 0:
                print(fuel_produced)
        for output, qty in qtys.items():
            if qty < 0:
                (prodqty, reqs) = inp[output]
                nrx = math.ceil(abs(qty) / prodqty)
                qtys[output] += prodqty * nrx
                for (amt, typ) in reqs:
                    if typ == "ORE":
                        ore_needed += amt * nrx
                        if ore_needed >= 1000000000000:
                            return fuel_produced - 1
                    else:
                        qtys[typ] -= amt * nrx
                break
        if max(qtys.values()) == 0 and min(qtys.values()) == 0:
            break
    print(f"{ore_needed} ore produces {fuel_produced} fuel")
    fuel_produced *= (1000000000000) // ore_needed
    ore_left = 1000000000000 % ore_needed
    while True:
        if min(qtys.values()) >= 0:
            qtys["FUEL"] = -1
            fuel_produced += 1
        for output, qty in qtys.items():
            if qty < 0:
                (prodqty, reqs) = inp[output]
                nrx = math.ceil(abs(qty) / prodqty)
                qtys[output] += prodqty * nrx
                for (amt, typ) in reqs:
                    if typ == "ORE":
                        ore_left -= amt * nrx
                        if ore_left < 0:
                            return fuel_produced - 1
                    else:
                        qtys[typ] -= amt * nrx
                break
    

def run(filename):
    inp = read_input(filename)
    print(part1(inp))
    print(1000000000000)
    print(part2(inp))

if __name__ == "__main__":
    run(sys.argv[1])
