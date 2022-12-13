#! /usr/bin/env pypy3.9

grid = []

try :
    while True :
        grid.append([int(c) for c in input()])
except EOFError :
    pass

visible = set()
for i in range(len(grid)) :
    visible.add((i, 0))
    visible.add((i, len(grid[0]) - 1))
for i in range(len(grid[0])) :
    visible.add((0, i))
    visible.add((len(grid) - 1, i))

for i in range(1, len(grid[0]) - 1) :
    for j in range(1, len(grid) - 1) :
        isVisible = \
            all(grid[i][y] < grid[i][j] for y in range(j)) or \
            all(grid[i][y] < grid[i][j] for y in range(len(grid[0]) - 1, j, -1)) or \
            all(grid[x][j] < grid[i][j] for x in range(i)) or \
            all(grid[x][j] < grid[i][j] for x in range(len(grid) - 1, i, -1))

        if isVisible : visible.add((i, j))

# part 1
print(len(visible))

scenicScore = []

for x in range(1, len(grid[0]) - 1) :
    for y in range(1, len(grid) - 1) :
        l, r, u, d = 0, 0, 0, 0
        for i in range(x - 1, -1, -1) :
            u += 1
            if grid[i][y] >= grid[x][y] :
                break
        for i in range(x + 1, len(grid), 1) :
            d += 1
            if grid[i][y] >= grid[x][y] :
                break
        for j in range(y - 1, -1, -1) :
            l += 1
            if grid[x][j] >= grid[x][y] :
                break
        for j in range(y + 1, len(grid)) :
            r += 1
            if grid[x][j] >= grid[x][y] :
                break

        if l == 0 : l = 1
        if r == 0 : r = 1
        if u == 0 : u = 1
        if d == 0 : d = 1

        scenicScore.append(l * r * u * d)
# part 2
print(max(scenicScore))
