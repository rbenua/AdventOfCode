#!/usr/bin/env python

import sys

i = 0
with open(sys.argv[1], 'r') as f:
    for line in f:
        i = i + int(line)
print i
