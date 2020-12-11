#! /usr/bin/env python3

def checkSide(board, x, y, side) -> chr :
    sides = {
        'L' : [-1, 0],
        'R' : [1, 0],
        'U' : [0, -1],
        'D' : [0, 1],
        'UL' : [-1, -1],
        'UR' : [1, -1],
        'DL' : [-1, 1],
        'DR' : [1, 1]
    }

    lx, ly = x, y

    while ly < len(board[0]) and ly >= 0 and lx < len(board) and lx >= 0 :
        if not(lx == x and ly == y) and board[lx][ly] != '.' :
            return board[lx][ly]

        lx += sides[side][0]
        ly += sides[side][1]
    return '.'

def neighbors(board, x, y) -> int :
    run = ['L', 'R', 'U', 'D', 'UL', 'UR', 'DL', 'DR']
    checks = [checkSide(board, x, y, side) == '#' for side in run]

    return sum(checks)

def nextBoard(board) :
    newBoard = [x[:] for x in board]

    for i in range(len(board)) :
        for j in range(len(board[0])) :
            if board[i][j] == 'L' and neighbors(board, i, j) == 0 :
                newBoard[i][j] = '#'
            elif board[i][j] == '#' and neighbors(board, i, j) >= 5 :
                newBoard[i][j] = 'L'

    return newBoard

def occupiedSeats(board) -> int :
    total = 0
    for row in board :
        total += row.count('#')
    return total

def printBoard(board) -> None :
    for row in board :
        print(*row)
    print("\n")

with open("input", "r") as fd :
    board = [[y for y in x] for x in fd.read().strip().split('\n')]

lastBoard = None

while lastBoard != board :
    lastBoard = board
    board = nextBoard(board)

print(f"Total occupied seats : {occupiedSeats(board)}")