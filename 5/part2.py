#! /usr/bin/env python3

fd = open("input", "r")
ids = []

for line in fd.readlines() :
    # Convert to binary
    row = int(line[:7].replace('B', '1').replace('F', '0'), 2)
    col = int(line[7:].replace('R', '1').replace('L', '0'), 2)

    # Calculate ID
    ids.append(row * 8 + col)

# O(nlog(n)) with sort.
ids.sort()

for i in range(len(ids) - 1) :
    if ids[i + 1] - ids[i] == 2 :
        print("Your ID :", ids[i] + 1)
        break