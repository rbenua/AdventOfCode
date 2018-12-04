#!/usr/bin/env python3

import sys
import re

guardpat = re.compile('Guard #(\d+) begins shift')
sleeppat = re.compile(':(\d\d)] falls asleep')
wakepat = re.compile(':(\d\d)] wakes up')
counts = {}

with open(sys.argv[1], 'r') as f:
    lines = f.read().splitlines()
    lines.sort()

curr_guard = 0
last_sleep = 0
guard_sleep = {}

for line in lines:
    guardmatch = guardpat.search(line)
    if guardmatch:
        curr_guard = int(guardmatch.group(1))
        continue

    sleepmatch = sleeppat.search(line)
    if sleepmatch:
        last_sleep = int(sleepmatch.group(1))
        continue
   
    wakematch = wakepat.search(line)
    if not wakematch:
        print("couldn't match on {}!".format(line))
        exit(1)

    waketime = int(wakematch.group(1))
    (curr_count, curr_data) = guard_sleep.get(curr_guard, (0, {}))
    
    for i in range(last_sleep, waketime):
        curr_data[i] = curr_data.get(i, 0) + 1
        if curr_data[i] > curr_count:
            curr_count = curr_data[i]

    guard_sleep[curr_guard] = (curr_count, curr_data)

max_count = 0
max_id = 0
for (guard, (count, data)) in guard_sleep.items(): 
    if count > max_count:
        max_count = count 
        max_id = guard

max_min = 0
(time, data) = guard_sleep[max_id]
for (min, count) in data.items():
    if count == max_count:
        max_min = min
        break        

print(max_id)
print(max_min)
print(max_id * max_min)
