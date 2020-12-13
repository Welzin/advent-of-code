#! /usr/bin/env python3

def closest(x, base):
    return base * round(x/base)

with open("input", "r") as fd :
    content = fd.read().strip().split('\n')
    depart  = int(content[0])
    bus     = [int(x) for x in content[1].split(',') if x != 'x']

nearest, id = 2**32, 0

for b in bus :
    c = closest(depart, b)
    if nearest > c >= depart :
        nearest = c
        id = b

print(f"ID * minutes = {(nearest - depart) * id}")