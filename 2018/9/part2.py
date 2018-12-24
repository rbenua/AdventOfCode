#!/usr/bin/env python3

import sys

class Node:
    def __init__(self, value):
        self.prev = self
        self.next = self
        self.value = value

    def insert(self, value):
        new = Node(value)
        new.prev = self
        new.next = self.next
        self.next.prev = new
        self.next = new
        return new
    
    def remove(self):
        self.next.prev = self.prev
        self.prev.next = self.next
        return (self.next, self.value)

    def fseek(self, i):
        if i == 0:
            return self
        return self.next.fseek(i-1)

    def bseek(self, i):
        if i == 0:
            return self
        return self.prev.bseek(i-1)

players = int(sys.argv[1])
marbles = int(sys.argv[2]) + 1

scores = [0] * players

curr = Node(0)
zero = curr

def printlist():
    n = zero
    print("{:2}{}".format(n.value, (" ","*")[n == curr]), end="")
    n = n.next
    while n != zero:
        print("{:2}{}".format(n.value, (" ","*")[n == curr]), end="")
        n = n.next
    print()

for marble in range(1, marbles):
    if marble % 1000 == 0:
        print("processed {} marbles".format(marble))
    player = marble % players
    if marble % 23 == 0:
        curr = curr.bseek(7)
        (curr, removed) = curr.remove()
        scores[player] += removed + marble
        #print("player {} kept {} and removed {}, total score {}".format(player, marble, removed, scores[player]))
    else:
        curr = curr.fseek(1)
        curr = curr.insert(marble)
    #printlist()

print(max(scores))
