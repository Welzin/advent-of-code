#! /usr/bin/env python3

def getBag(line) :
    words = line.split()
    return " ".join((words[0], words[1]))

import re

content = open("input", "r").read().split('\n')
bags    = [getBag(x) for x in content if re.search(r"\d shiny gold", x) != None]
total   = len(bags)
found   = bags

while len(bags) > 0 :
    curr = []
    for bag in bags :
        curr.extend([getBag(x) for x in content if re.search(rf"\d {bag}", x) != None and getBag(x) not in found])
    bags = set(curr)
    found.extend(set(curr))
    total += len(bags)

print(total)