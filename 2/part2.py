#! /usr/bin/python3

import re

fd = open("input", "r")

valid = 0

for line in fd.readlines() :
    i, j, char, string = re.search(r"(\d+)-(\d+) (.): (.*)", line).groups()

    a = string[int(i) - 1] == char
    b = string[int(j) - 1] == char

    if bool(a) ^ bool(b) :
        valid += 1

print(valid)
