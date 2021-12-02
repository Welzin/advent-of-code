#! /usr/bin/env python3

entries = [(line.split()[0], int(line.split()[1])) for line in open("input")]
print(sum(x for d, x in entries if d == 'forward') * sum(x*(d == 'down')-x*(d == 'up') for d, x in entries))