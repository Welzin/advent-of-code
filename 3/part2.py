#! /usr/bin/python3

fd = open("input", "r")

counts = [0] * 5
cols = [0] * 5
lines = 0
adds = [1, 3, 5, 7, 1]

for line in fd.readlines() :
    for i in range(4) :
        counts[i] += line[cols[i] % (len(line) - 1)] == '#'
        cols[i] += adds[i]

    if lines % 2 == 0 :
        counts[-1] += line[cols[-1] % (len(line) - 1)] == '#'
        cols[-1] += adds[-1]

    lines += 1

product = 1
for x in counts :
    product *= x

print(product)
