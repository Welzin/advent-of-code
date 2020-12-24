#! /usr/bin/env python3

from typing import List, Tuple, T


class Hex :
    x: int
    y: int
    z: int

    def __init__(self, x: int, y: int, z: int) -> None :
        self.x = x
        self.y = y
        self.z = z

    def __eq__(self, oth) -> bool :
        return self.x == oth.x and self.y == oth.y and self.z == oth.z

    def __repr__(self) -> str :
        return f"Hex({self.x}, {self.y}, {self.z})"

def hexAdd(a: Hex, b: Hex) -> Hex :
    return Hex(a.x + b.x, a.y + b.y, a.z + b.z)

def hexDirection(direction: int) -> Hex :
    return hexDirections[direction]

def hexNeighbor(hex: Hex, direction: int) -> Hex :
    return hexAdd(hex, hexDirection(direction))

hexDirections = [Hex(1, 0, -1), Hex(1, -1, 0), Hex(0, -1, 1), Hex(-1, 0, 1), Hex(-1, 1, 0), Hex(0, 1, -1)]
code = { "ne": 0, "e": 1, "se": 2, "sw": 3, "w": 4, "nw": 5 }

with open("input", "r") as fd :
    tiles = fd.read().split('\n')

black = []

for tile in tiles :
    curr, i = Hex(0, 0, 0), 0
    while i < len(tile) :
        if tile[i] == 's' or tile[i] == 'n' :
            curr = hexNeighbor(curr, code[tile[i:i+2]])
            i += 2
        else :
            curr = hexNeighbor(curr, code[tile[i]])
            i += 1

    if curr not in black :
        black.append(curr)
    else : 
        black.remove(curr)

print(len(black))
