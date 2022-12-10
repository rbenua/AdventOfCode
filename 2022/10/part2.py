#!/usr/bin/env python3

import sys
import re
f = open(sys.argv[1], "r")

insns = []
for line in f:
    if line.strip() == "noop":
        insns.append(None)
    else:
        insns.append(int(line.strip().split()[1]))

x = 1
history = []

for insn in insns:
    if insn is None:
        history.append(x)
    else:
        history.append(x)
        history.append(x)
        x += insn

for row in range(6):
    for col in range(40):
        idx = row * 40 + col
        if abs(history[idx] - col) <= 1:
            print("#", end="")
        else:
            print(" ", end="")
    print()
