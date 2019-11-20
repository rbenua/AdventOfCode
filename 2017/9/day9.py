#!/usr/bin/env python3

import sys, re, collections

def read_input(filename):
    with open(filename, 'r') as f:
        return f.read().strip()

def find_garbage_end(stream, cursor):
    while stream[cursor] != '>':
        if stream[cursor] == '!':
            cursor += 1
        cursor += 1
    return cursor + 1

def score_group(stream, cursor, depth):
    total = depth
    while cursor < len(stream):
        if stream[cursor] == '<':
            cursor = find_garbage_end(stream, cursor)
        elif stream[cursor] == ',':
            cursor += 1
        elif stream[cursor] == '{':
            (s, nc) = score_group(stream, cursor + 1, depth + 1)
            total += s
            cursor = nc
        elif stream[cursor] == '}':
            return (total, cursor + 1)
        else:
            print(f"unexpected character {stream[cursor]} at position {cursor} in group context!")
            exit(1)
    return (total, cursor)


def part1(stream):
    return score_group(stream, 0, 0)[0]

def part2(stream):
    counting = False
    canceled = False
    total = 0
    for c in stream:
        if counting:
            if canceled:
                canceled = False
            elif c == '!':
                canceled = True
            elif c == '>':
                counting = False
            else:
                total += 1
        elif canceled:
            canceled = False
        elif c == '!':
            canceled = True
        elif c == '<':
            counting = True
    return total


def run(filename):
    stream = read_input(filename)
    print(part1(stream))
    print(part2(stream))

if __name__ == "__main__":
    run(sys.argv[1])
