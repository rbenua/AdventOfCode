#!/usr/bin/env python3

import sys

inlen = 0
board = [3, 7]
currs = list(range(2))

def expand():
    total = sum([board[c] for c in currs])
    for c in str(total):
        board.append(int(c)) # this is absurd
    
def choose_new():
    for i in range(len(currs)):
        currs[i] = (currs[i] + board[currs[i]] + 1) % len(board)

def run():
    while len(board) < inlen + 10:
        expand()
        choose_new()
    print(''.join([str(i) for i in board[inlen:inlen+10]]))

if __name__ == "__main__":
    inlen = int(sys.argv[1])
    run()
