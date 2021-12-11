#! /usr/bin/env python3

def neighbors(i, j) :
    return [(x, y) for x in range(i - 1, i + 2) for y in range(j - 1, j + 2) if 0 <= x < 10 and 0 <= y < 10 and (x != i or y != j)]

def process(octopuses, steps, flashes) :
    to_flash = []
    for i in range(10) :
        for j in range(10) :
            octopuses[i][j] += 1
            if octopuses[i][j] > 9 : to_flash += [(i, j)]

    for x, y in to_flash :
        for a, b in neighbors(x, y) :
            if (a, b) not in to_flash :
                octopuses[a][b] += 1
                if octopuses[a][b] > 9 : to_flash += [(a, b)]

    for x, y in to_flash :
        octopuses[x][y] = 0

    if len(to_flash) == 100 :
        return steps + 1

    return process(octopuses, steps + 1, flashes + len(to_flash))

print(process([[int(c) for c in line.strip()] for line in open("input")], 0, 0))