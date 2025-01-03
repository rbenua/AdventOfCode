#!/usr/bin/env python3

import sys
import re
f = open(sys.argv[1], "r")

def nums(s):
    return [int(x) for x in re.findall(r'(-?\d+).?', s)]

s = f.read().strip();

for i in range(len(s) - 14):
    window = set(s[i:i+14])
    
    if len(window) == 14:
        print(i + 14)
        break
