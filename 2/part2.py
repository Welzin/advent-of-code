#! /usr/bin/python3

import re

fd = open("input", "r")

valid = 0

for line in fd.readlines() :
    match = re.findall(r"(\d+)-(\d+) (.): (.*)", line)[0]

    a = match[3][int(match[0]) - 1] == match[2]
    b = match[3][int(match[1]) - 1] == match[2]

    if bool(a) ^ bool(b) :
        valid += 1

print(valid)
