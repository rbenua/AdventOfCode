#!/usr/bin/env python3

import sys, re, collections

pat = re.compile("(\w+) \((\d+)\)(?: -> (.*))?\n")

def read_input(filename):
    with open(filename, 'r') as f:
        return [line for line in f]

def part1(lines):
    seen_rhs = set()
    seen_lhs = set()
    d = {}
    for line in lines:
        match = pat.match(line)
        seen_lhs.add(match.group(1))
        rhs = match.group(3)
        if rhs:
            seen_rhs |= set(rhs.split(", "))
            d[match.group(1)] = (int(match.group(2)), rhs.split(", "))
        else:
            d[match.group(1)] = (int(match.group(2)), None)
    return ((seen_lhs - seen_rhs).pop(), d)


shouldprint = True
def part2(root, tree):
    global shouldprint
    (selfwt, children) = tree[root]
    if not children:
        return selfwt
    childwts = [part2(child, tree) for child in children]
    if (min(childwts) != max(childwts)) and shouldprint:
        print("Found mismatch: ", list(zip(children, childwts)))
        shouldprint = False
    return selfwt + sum(childwts)

def run(filename):
    lines = read_input(filename)
    (root, tree) = part1(lines)
    print(root)
    print(part2(root, tree))

if __name__ == "__main__":
    run(sys.argv[1])
