#! /usr/bin/env python3

def neighbors(board, x, y) -> int :
    minx, maxx = x - 1, x + 1
    miny, maxy = y - 1, y + 1

    if miny < 0 : miny = 0
    if maxy >= len(board[0]) : maxy = len(board[0]) - 1
    if minx < 0 : minx = 0
    if maxx >= len(board) : maxx = len(board) - 1

    count = 0

    for i in range(minx, maxx + 1) :
        for j in range(miny, maxy + 1) :
            if not(x == i and y == j) and board[i][j] == '#' :
                count += 1
    return count

def nextBoard(board) :
    newBoard = [x[:] for x in board]

    for i in range(len(board)) :
        for j in range(len(board[0])) :
            if board[i][j] == 'L' and neighbors(board, i, j) == 0 :
                newBoard[i][j] = '#'
            elif board[i][j] == '#' and neighbors(board, i, j) >= 4 :
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