#! /usr/bin/env python3

def add_points(a, b, points) :
    incr_x, incr_y = (a[0] != b[0]) * (-2 * (a[0] > b[0]) + 1), (a[1] != b[1]) * (-2 * (a[1] > b[1]) + 1)
    x, y = a[0], a[1]
    while (x, y) != (b[0] + incr_x, b[1] + incr_y) :
        if (x, y) in points : points[(x, y)] += 1
        else : points[(x, y)] = 1
        x += incr_x
        y += incr_y

    return points

points = {}
for line in open("input") :
    (xa, ya), (xb, yb) = map(lambda x: map(int, x.split(',')), line.split('->'))
    if xa == xb or ya == yb :
        points = add_points((xa, ya), (xb, yb), points)

print(sum(value > 1 for value in points.values()))