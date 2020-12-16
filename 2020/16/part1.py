#! /usr/bin/env python3

import re

def getValidValues(fields) :
    valid = []
    for field in fields :
        match = re.match(r"^.*: (\d+-\d+) or (\d+-\d+)$", field).groups()
        valid += [match[0].split('-'), match[1].split('-')]
    return valid


with open("input", "r") as fd :
    content = [x.split('\n') for x in fd.read().strip().split('\n\n')]

rules = getValidValues(content[0])
total = 0
for ticket in content[2][1:] :
    nums = [int(x) for x in ticket.split(',')]
    for num in nums :
        i, found = 0, False
        while i < len(rules) and not found :
            rule = rules[i]
            found = int(rule[0]) <= num <= int(rule[1])
            i += 1
        if not found :
            total += num
print(f"Sum of invalid nums : {total}")
