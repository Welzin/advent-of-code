#! /usr/bin/env python3

fd = open("input", "r")
max = 0

for line in fd.readlines() :
    # Convert to binary
    row = int(line[:7].replace('B', '1').replace('F', '0'), 2)
    col = int(line[7:].replace('R', '1').replace('L', '0'), 2)

    # Calculate ID
    id  = row * 8 + col
    if id > max :
        max = id

print("Max :", max) 