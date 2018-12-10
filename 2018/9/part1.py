#!/usr/bin/env python3

import sys

players = int(sys.argv[1])
marbles = int(sys.argv[2]) + 1

scores = [0] * players

ring = [0]
curr = 0

for marble in range(1, marbles):
    player = marble % players
    if marble % 23 == 0:
        curr = (curr - 7) % len(ring)
        removed = ring.pop(curr)
        scores[player] += removed + marble
    else:
        curr = (curr + 2) % len(ring)
        ring.insert(curr, marble)

print(max(scores))
