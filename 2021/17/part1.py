#! /usr/bin/env python3

import re 

def trajectory(x, y, xmin, xmax, ymin, ymax) :
    start, max_y_value = (0, 0), 0
    while (start[0] < xmin or start[1] > ymax) and not(start[0] > xmax or start[1] < ymin) :
        start = (start[0] + x, start[1] + y)
        x, y = 0 if x == 0 else (abs(x)-1)*(abs(x)//x), y - 1
        if start[1] > max_y_value : max_y_value = start[1]
    if xmin <= start[0] <= xmax and ymin <= start[1] <= ymax :
        return max_y_value
    return 0

xmin, xmax, ymin, ymax = map(int, re.findall(r"(-?\d+)", open("input").readline()))
print(max(trajectory(a, y, xmin, xmax, ymin, ymax) for a in range(xmax+1) for y in range(ymin, -ymin+1)))