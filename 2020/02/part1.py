#! /usr/bin/env python3

import re

fd = open("input", "r")

valid = 0

for line in fd.readlines() :
    least, max, char, string = re.search(r"(\d+)-(\d+) (.): (.*)", line).groups()

    if int(least) <= string.count(char) <= int(max) :
        valid += 1

print(valid)
