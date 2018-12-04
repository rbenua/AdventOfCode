#!/usr/bin/env python3

import sys

seen = {0}
prev = 0
with open(sys.argv[1], 'r') as f:
    while True:
        for line in f:
            delta = int(line)
            curr = prev + delta
            if curr in seen:
                print(curr)
                sys.exit(0)
            seen.add(curr)
            #print('adding {}, total {}, seen {}'.format(delta, curr, seen))
            prev = curr
        f.seek(0)

