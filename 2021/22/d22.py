#! /usr/bin/env python3

import re, collections

def solve(lines, part1 = True) :
    cubes = collections.Counter()
    for line in lines :
        shall_turn_on = line.split()[0] == "on"
        xmin, xmax, ymin, ymax, zmin, zmax = map(int, re.findall("-?\d+", line))

        if part1 and not(-50 <= xmin <= 50 and -50 <= xmax <= 50 and -50 <= ymin <= 50 and -50 <= ymax <= 50 and -50 <= zmin <= 50 and -50 <= zmax <= 50) : continue 

        update = collections.Counter()
        for (x0, x1, y0, y1, z0, z1), status in cubes.items():
            ix0 = max(xmin, x0); ix1 = min(xmax, x1)
            iy0 = max(ymin, y0); iy1 = min(ymax, y1)
            iz0 = max(zmin, z0); iz1 = min(zmax, z1)
            if ix0 <= ix1 and iy0 <= iy1 and iz0 <= iz1:
                update[(ix0, ix1, iy0, iy1, iz0, iz1)] -= status

        if shall_turn_on :
            update[(xmin, xmax, ymin, ymax, zmin, zmax)] += 1
        cubes.update(update)

    print(sum((x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1) * sgn for (x0, x1, y0, y1, z0, z1), sgn in cubes.items()))

solve(open("22.txt").readlines())
solve(open("22.txt").readlines(), False)