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

last, not_valids = 0, boards[:]
for draw in draws :
    last = draw
    for i in range(len(not_valids)) :
        not_valids[i] = delete(draw, not_valids[i])

    if len(not_valids) == 1 :
        if valid(not_valids[0]) :
            board = not_valids[0]
            sm = sum([sum(board[i]) + board[i].count(-1) for i in range(len(board))])
            break
    else :
        for i in range(len(not_valids)) :
            if valid(not_valids[i]) : not_valids[i] = []
        not_valids = [board for board in not_valids if board]
print(sm*last)