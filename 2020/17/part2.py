#! /usr/bin/env python3

from typing import List

class Cube :
    def __init__(self, state, x, y, z, w) -> None :
        self.x = x
        self.y = y
        self.z = z
        self.w = w
        self.isAlive = state

    def __repr__(self) :
        return f"({self.x}, {self.y}, {self.z}, {self.w}): {self.isAlive}"

class ConwayCubes :
    def __init__(self, nativeState: List[List[chr]]) -> None :
        self.cubes = []
        for i, ls in enumerate(nativeState) :
            for j, c in enumerate(ls) :
                if c == '#' :
                    self.cubes.append(Cube(True, i, j, 0, 0))

    def exists(self, x: int, y: int, z: int, w: int) -> Cube :
        for cube in self.cubes :
            if cube.x == x and cube.y == y and cube.z == z and cube.w == w and cube.isAlive :
                return cube
        return None

    def neighbors(self, x: int, y: int, z: int, w: int) -> int :
        count = 0
        for i in range(x - 1, x + 2) :
            for j in range(y - 1, y + 2) :
                for k in range(z - 1, z + 2) :
                    for l in range(w - 1, w + 2) :
                        if not(x == i and y == j and z == k and w == l) and self.exists(i, j, k, l) != None :
                            count += 1
        return count

    def bounds(self) -> int :
        minx, maxx = miny, maxy = minz, maxz = minw, maxw = 2**16, -(2**16)
        for cube in self.cubes :
            if cube.x < minx : minx = cube.x
            if cube.x > maxx : maxx = cube.x
            if cube.y < miny : miny = cube.y
            if cube.y > maxy : maxy = cube.y
            if cube.z < minz : minz = cube.z
            if cube.z > maxz : maxz = cube.z
            if cube.w < minw : minw = cube.w
            if cube.w > maxw : maxw = cube.w
        return (minx, maxx, miny, maxy, minz, maxz, minw, maxw)

    def add(self, cube: Cube) -> None :
        self.cubes.append(cube)

    def next(self) :
        nextBoard = ConwayCubes([[]])
        minx, maxx, miny, maxy, minz, maxz, minw, maxw = self.bounds()
        for i in range(minx - 1, maxx + 2) :
            for j in range(miny - 1, maxy + 2) :
                for k in range(minz - 1, maxz + 2) :
                    for l in range(minw - 1, maxw + 2) :
                        cube = self.exists(i, j, k, l)
                        if cube == None and self.neighbors(i, j, k, l) == 3 :
                            nextBoard.add(Cube(True, i, j, k, l))
                        elif cube != None and 2 <= self.neighbors(i, j, k, l) <= 3 :
                            nextBoard.add(cube)
                        
        return nextBoard

    def alive(self) -> int :
        return sum(cube.isAlive for cube in self.cubes)


# 223

with open("input", "r") as fd :
    z = [[c for c in x] for x in fd.read().strip().split('\n')]

cc = ConwayCubes(z)
for i in range(6) :
    print(i)
    cc = cc.next()

print(f"Number of active cubes : {cc.alive()}")