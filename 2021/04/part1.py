#! /usr/bin/env python3

entries = [line.strip() for line in open("input") if line.strip()]
draws, entries = list(map(int, entries[0].split(','))), entries[1:]
boards = [[list(map(int, entries[j].split())) for j in range(i*5, (i*5)+5)] for i in range(len(entries)//5)]

def delete(n, board) :
    for i in range(len(board[0])) :
        for j in range(len(board)) :
            if board[i][j] == n : board[i][j] = -1
    return board

def valid(board) :
    for i in range(len(board)) :
        if sum(board[i]) == -5 : return True 
        if sum([board[j][i] for j in range(len(board))]) == -5 : return True 
    return False

last = 0
for draw in draws :
    last = draw
    for i in range(len(boards)) :
        boards[i] = delete(draw, boards[i])

    mx = 0
    for board in boards :
        if valid(board) :
            sm = sum([sum(board[i]) + board[i].count(-1) for i in range(len(board))])
            if sm > mx :
                mx = sm
    if mx > 0 : break
print(mx * last)