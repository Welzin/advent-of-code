#! /usr/bin/env python3

from typing import List, Tuple, T, Dict


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

    def __hash__(self) :
        return hash((self.x, self.y, self.z))

class HexFlip :
    tiles: List[Hex]
    hexDirections: List[Hex]
    hexCode: Dict[str, int]

    def __init__(self, instructions) :
        self.hexDirections = [Hex(1, 0, -1), Hex(1, -1, 0), Hex(0, -1, 1), Hex(-1, 0, 1), Hex(-1, 1, 0), Hex(0, 1, -1)]
        self.code = { "ne": 0, "e": 1, "se": 2, "sw": 3, "w": 4, "nw": 5 }
        self.tiles = []
        self.first(instructions)

    def hexAdd(self, a: Hex, b: Hex) -> Hex :
        return Hex(a.x + b.x, a.y + b.y, a.z + b.z)

    def hexDirection(self, direction: int) -> Hex :
        return self.hexDirections[direction]

    def hexNeighbor(self, hex: Hex, direction: int) -> Hex :
        return self.hexAdd(hex, self.hexDirection(direction))

    def next(self) -> None :
        neighbors = {}
        delete    = set()
        add       = set()
        for tile in self.tiles :
            count = 0
            for i in range(6) :
                n = self.hexNeighbor(tile, i)
                if n in self.tiles : count += 1
                neighbors[n] = neighbors.get(n, 0) + 1
            if count == 0 or count > 2 : delete.add(tile)
        for k, v in neighbors.items() :
            if v == 2 and k not in self.tiles : add.add(k)

        for d in delete : self.tiles.remove(d)
        for a in add    : self.tiles.append(a)

    def first(self, instructions) :
        for tile in instructions :
            curr, i = Hex(0, 0, 0), 0
            while i < len(tile) :
                if tile[i] == 's' or tile[i] == 'n' :
                    curr = self.hexNeighbor(curr, self.code[tile[i:i+2]])
                    i += 2
                else :
                    curr = self.hexNeighbor(curr, self.code[tile[i]])
                    i += 1

            if curr not in self.tiles :
                self.tiles.append(curr)
            else : 
                self.tiles.remove(curr)


with open("input", "r") as fd :
    tiles = fd.read().split('\n')

flip = HexFlip(tiles)
for i in range(100) :
    print(f"{i + 1}/100")
    flip.next()

print(len(flip.tiles))
