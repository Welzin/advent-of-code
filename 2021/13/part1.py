#! /usr/bin/env python3

lines = [line.strip() for line in open("input")]
points = set(tuple(map(int, line.split(',')[::-1])) for line in lines[:lines.index('')])
folds = [(line.split('=')[0][-1:], int(line.split('=')[1])) for line in lines[lines.index('')+1:]]

for axis, above in folds[:1] :
    foldedPoints = set()
    for x, y in points :
        if axis == 'y' and x > above :
            x = 2*above - x
        if axis == 'x' and y > above :
            y = 2*above - y
        
        if x >= 0 and y >= 0 :
            foldedPoints.add((x, y))
    points = foldedPoints

print(len(points))