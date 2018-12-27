#!/usr/bin/env python3

import sys

with open(sys.argv[1], 'r') as f:
    initchars = list(f.read().strip())

def process(c, ichars):
    print("removing ", c)
    chars = ichars.copy()
    i = len(chars) - 1
    while i >= 0:
        if chars[i].lower() == c:
            del chars[i]
            if i >= len(chars):
                i = len(chars) - 1
        elif i > 0 and chars[i] != chars[i-1] and chars[i].lower() == chars[i-1].lower():
            del chars[i-1:i+1]
            if i >= len(chars):
                i = len(chars) - 1
        else:
            i -= 1
#    print(chars)
#    print(len(chars))
    return len(chars)

alpha = list('abcdefghijklmnopqrstuvwxyz')
print(min([process(c, initchars) for c in alpha]))
