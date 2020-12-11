#! /usr/bin/env python3

import re

def recurse(content, bag) :
    line = [x for x in content if re.search(rf"^{bag}", x) != None][0]
    bags = re.findall(r"(\d) ([^ ]+ {1}[^ ]+) ", line)

    if len(bags) == 0 :
        return 0

    return sum([int(bag[0]) + int(bag[0]) * recurse(content, bag[1]) for bag in bags])

with open("input", "r") as fd :
    print(recurse(fd.read().split('\n'), "shiny gold"))