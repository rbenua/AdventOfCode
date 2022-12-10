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

total = 0
for n in range(19, 220, 40):
    print(n + 1, history[n] * (n + 1))
    total += history[n] * (n + 1)

print(total)
