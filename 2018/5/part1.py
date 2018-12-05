#!/usr/bin/env python3

import sys

with open(sys.argv[1], 'r') as f:
    chars = list(f.read().strip())

i = 0
while i < len(chars) - 1:
    if chars[i] != chars[i+1] and chars[i].lower() == chars[i+1].lower():
        del chars[i:i+2]
        if i > 0:
            i -= 1
    else:
        i += 1

#print(chars)
print(len(chars))
