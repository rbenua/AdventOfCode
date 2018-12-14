#!/usr/bin/env python3

import sys

pat = 0
patlen = 0
searched = 0
board = [3, 7]
curr1 = 0
curr2 = 1

def expand():
    global curr1, curr2
    total = board[curr1] + board[curr2]
    if(total > 9):
        board.append(1)
    board.append(total % 10)
    
def choose_new():
    global curr1, curr2
    curr1 = (curr1 + board[curr1] + 1) % len(board)
    curr2 = (curr2 + board[curr2] + 1) % len(board)

def search():
    global searched
    for i in range(searched, len(board) - patlen):
        if board[i:i+patlen] == pat:
            print(i)
            return True
    searched = len(board) - patlen
    return False

def run():
    global pat, patlen, searched
    while not search():
        expand()
        choose_new()

if __name__ == "__main__":
    pat = [int(c) for c in sys.argv[1]]
    patlen = len(pat)
    run()
