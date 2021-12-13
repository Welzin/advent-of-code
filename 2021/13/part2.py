#! /usr/bin/env python3

lines = [line.strip() for line in open("input")]
points = set(tuple(map(int, line.split(',')[::-1])) for line in lines[:lines.index('')])
folds = [(line.split('=')[0][-1:], int(line.split('=')[1])) for line in lines[lines.index('')+1:]]

for axis, above in folds :
    foldedPoints = set()
    for x, y in points :
        if axis == 'y' and x > above :
            x = 2*above - x
        if axis == 'x' and y > above :
            y = 2*above - y
        
        if x >= 0 and y >= 0 :
            foldedPoints.add((x, y))
    points = foldedPoints

maxx = max(x for x, _ in points) + 1
maxy = max(y for _, y in points) + 1
repr = [['.'] * maxy for _ in range(maxx)]

for x, y in points :
    repr[x][y] = '#'

print('\n'.join(''.join(r) for r in repr))