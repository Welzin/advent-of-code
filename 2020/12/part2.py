#! /usr/bin/env python3

def rotate(dx, dy, dir, degrees) -> (int, int) :
    if dir == 'R' :
        for _ in range(degrees // 90) :
            dx, dy = dy, -dx
    elif dir == 'L' :
        for _ in range(degrees // 90) :
            dx, dy = -dy, dx
    return (dx, dy)

with open("input", "r") as fd :
    directions = [(x[0], int(x[1:])) for x in fd.read().strip().split('\n')]

dirs = { 'N': (0, 1), 'E': (1, 0), 'S': (0, -1), 'W': (-1, 0) }
x = y = 0
dx, dy = (10, 1)

for d in directions :
    if d[0] == 'L' or d[0] == 'R' :
        dx, dy = rotate(dx, dy, d[0], d[1])
    elif d[0] == 'F' :
        x += dx * d[1]
        y += dy * d[1]
    else :
        ddx, ddy = dirs[d[0]]
        dx += ddx * d[1]
        dy += ddy * d[1]

print(f"Manhattan distance : {abs(x) + abs(y)}")