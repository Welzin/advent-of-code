#! /usr/bin/python3

import re

fd = open("input", "r")

valid = 0

for line in fd.readlines() :
    match = re.findall(r"(\d+)-(\d+) (.): (.*)", line)[0]

    if int(match[0]) <= match[3].count(match[2]) <= int(match[1]) :
        valid += 1

print(valid)
