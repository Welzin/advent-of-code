#! /usr/bin/env python3

from copy import deepcopy
from time import time

def print_board(T) :
    print('\n'.join([''.join(T[i]) for i in range(len(T))]), end='\n\n')

T = []
for line in open("25.txt").readlines() :
    T.append([c for c in line.strip()])

st = time()
n, m = len(T), len(T[0])
moves, step = 1, 0
while moves > 0 :
    moves = 0
    cp = deepcopy(T)
    for i in range(n) :
        for j in range(m) :
            x, y = i, (j+1)%m
            if cp[i][j] == '>' and cp[x][y] == '.' :
                T[x][y], T[i][j] = '>', '.'
                moves += 1
    cp = deepcopy(T)
    for i in range(n) :
        for j in range(m) :
            x, y = (i+1)%n, j
            if cp[i][j] == 'v' and cp[x][y] == '.' :
                T[x][y], T[i][j] = 'v', '.'
                moves += 1
    step += 1

print(f"Solution: {step}, time: {time() - st}s")