#! /usr/bin/env python3

import copy
from typing import List
from math import sqrt

class Tile :
    id: int
    tile: List[List[chr]]
    witness: List[List[chr]]
    eta: int
    tot: int

    def __init__(self, tile: List[str]) -> None :
        self.id      = int(tile[0][tile[0].rfind(' '):-1].strip())
        self.tile    = [[c for c in w] for w in tile[1:]]
        self.witness = copy.deepcopy(self.tile)
        self.eta     = 0
        self.tot     = 0

    def top(self) -> str :
        return "".join(self.tile[0])
    
    def bottom(self) -> str :
        return "".join(self.tile[-1])

    def left(self) -> str :
        return "".join([x[0] for x in self.tile])

    def right(self) -> str :
        return "".join([x[-1] for x in self.tile])

    def flip(self) -> None :
        for i in range(len(self.tile) // 2) :
            tmp, self.tile[i] = self.tile[i], self.tile[-(i + 1)]
            self.tile[-(i + 1)] = tmp

    def rotate(self, times: int) -> None :
        assert(len(self.tile) == len(self.tile[0]))

        for _ in range(times) :
            cp = copy.deepcopy(self.tile)
            for i in range(len(self.tile)) :
                for j in range(len(self.tile)) :
                    self.tile[j][-(i + 1)] = cp[i][j]

    def next(self) -> None :
        if self.eta == 0 :
            self.flip()
            self.eta += 1
        if self.eta == 1 :
            self.tile = self.witness
            self.rotate(1)
            self.witness = self.tile
            self.eta = 0
            self.tot += 1

    def end(self) -> bool :
        return self.tot == 4

    def __repr__(self) -> str :
        return str(self.id) + '\n' + '\n'.join([''.join(x) for x in self.tile])

def backtrack(tiles: List[Tile]) -> List[List[Tile]] :
    n, board = int(sqrt(len(tiles))), []
    for i in range(n) :
        board.append([])
        for _ in range(n) :
            board[i].append(None)
    return bc(board, tiles)

def accept(c, x, y) :
    if x - 1 >= 0 and c[x - 1][y] is not None :
        if c[x][y].top() != c[x - 1][y].bottom() : return False
    if x + 1 < len(c) and c[x + 1][y] is not None :
        if c[x][y].bottom() != c[x + 1][y].top() : return False
    if y - 1 >= 0 and c[x][y - 1] is not None :
        if c[x][y].left() != c[x][y - 1].right() : return False
    if y + 1 < len(c[0]) and c[x][y + 1] is not None :
        if c[x][y].right() != c[x][y + 1].left() : return False
    return True

def bc(board: List[List[Tile]], tiles: List[Tile]) :
    i = 0
    q = copy.deepcopy(tiles)

    while i < len(board) :
        j = 0
        while j < len(board[i]) :
            board[i][j] = q.pop(0)
            print(i, j)
            denied, totallyDenied = 0, False
            while not totallyDenied and not accept(board, i, j) :
                if board[i][j].end() :
                    denied += 1
                    if denied == len(q) :
                        tmp = board[i][j]
                        board[i][j] = None
                        j -= 1 
                        if j < 0 :
                            i -= 1 
                            j = len(board[i])
                        q.append(tmp)
                        totallyDenied = True
                    else :
                        q.append(board[i][j])
                        board[i][j] = q.pop(0)    
                if board[i][j] is not None : board[i][j].next()
            if not totallyDenied : 
                j += 1
        i += 1
    return board

with open("test", "r") as fd :
    tiles = [Tile([w for w in x.split('\n')]) for x in fd.read().split('\n\n')]

print(backtrack(tiles))