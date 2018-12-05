#!/usr/bin/env python3

import sys

with open(sys.argv[1], 'r') as f:
    initchars = list(f.read().strip())

def process(c, ichars):
    print("removing ", c)
    chars = ichars.copy()
    i = 0
    while i < len(chars) - 1:
        if chars[i].lower() == c:
            del chars[i]
            if i > 0:
                i -= 1
        elif chars[i] != chars[i+1] and chars[i].lower() == chars[i+1].lower():
            del chars[i:i+2]
            if i > 0:
                i -= 1
        else:
            i += 1
#    print(chars)
#    print(len(chars))
    return len(chars)

alpha = list('abcdefghijklmnopqrstuvwxyz')
print(min([process(c, initchars) for c in alpha]))
