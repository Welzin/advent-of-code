#! /usr/bin/python3

fd = open("input", "r")

col = 0
count = 0

for line in fd.readlines() :
    count += line[col % (len(line) - 1)] == '#'
    col += 3

print(count)
