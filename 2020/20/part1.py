#! /usr/bin/env python3

import copy
from typing import List
from math import sqrt

class Tile :
    id: int
    tile: List[List[chr]]
    edges: List[List[List[chr]]]

    def __init__(self, tile: List[str]) -> None :
        self.id    = int(tile[0][tile[0].rfind(' '):-1].strip())
        self.tile  = [[c for c in w] for w in tile[1:]]
        self.edges = [
            self.top(), self.top()[::-1],
            self.right(), self.right()[::-1],
            self.bottom(), self.bottom()[::-1],
            self.left(), self.left()[::-1]
        ]

    def top(self) -> str :
        return "".join(self.tile[0])
    
    def bottom(self) -> str :
        return "".join(self.tile[-1])

    def left(self) -> str :
        return "".join([x[0] for x in self.tile])

    def right(self) -> str :
        return "".join([x[-1] for x in self.tile])

    def sim(self, oth) -> int :
        sum = 0
        for edge in self.edges :
            sum += edge in oth.edges
        return sum

with open("input", "r") as fd :
    tiles = [Tile([w for w in x.split('\n')]) for x in fd.read().split('\n\n')]

simEdges = {}

for i in range(len(tiles)) :
    tile = tiles[i]
    for j in range(len(tiles)) :
        if j != i :
            if tile.id not in simEdges :
                simEdges[tile.id] = 0
            simEdges[tile.id] += tile.sim(tiles[j])

i, prod = 0, 1
while i != 4 :
    minKey = min(simEdges.keys(), key=(lambda k: simEdges[k]))
    prod *= minKey
    simEdges[minKey] = 2**32
    i += 1
print(f"Product: {prod}")
