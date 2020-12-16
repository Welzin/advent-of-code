#! /usr/bin/env python3

import re

def getValidValues(fields) :
    valid = []
    for field in fields :
        match = re.match(r"^.*: (\d+-\d+) or (\d+-\d+)$", field).groups()
        valid += [(match[0].split('-'), match[1].split('-'))]
    return valid

def getMax(rules) :
    max, r = -1, -1
    for k, v in rules.items() :
        if len(v) > max :
            max, r = len(v), k
    return r

with open("input", "r") as fd :
    content = [x.split('\n') for x in fd.read().strip().split('\n\n')]

# Get valid tickets
rules, validTickets = getValidValues(content[0]), []
for ticket in content[2][1:] :
    nums, add = [int(x) for x in ticket.split(',')], True
    for j, num in enumerate(nums) :
        i, found = 0, False
        while i < len(rules) :
            rule = rules[i]
            if int(rule[0][0]) <= num <= int(rule[0][1]) or int(rule[1][0]) <= num <= int(rule[1][1]) :
                found = True 
            i += 1
        if not found :
            add = False
            break
    if add :
        validTickets.append(ticket)

# Verified rules stores the rule and the fields it can not be
verifiedRules = {}
for ticket in validTickets :
    nums = [int(x) for x in ticket.split(',')]
    for j, num in enumerate(nums) :
        i, found = 0, False
        while i < len(rules) :
            rule = rules[i]
            if not (int(rule[0][0]) <= num <= int(rule[0][1]) or int(rule[1][0]) <= num <= int(rule[1][1])) :
                verifiedRules[i] = verifiedRules.get(i, []) + [j]
            i += 1

available = [x for x in range(len(validTickets[0].split(',')))]
verifiedRules = {k: set(verifiedRules.get(k, [])) for k in range(len(available))}

# Calculate the fields it can be
ls, d = [], {}
max = len(available)
while len(d) < max :
    key = getMax(verifiedRules)
    ls = verifiedRules[key]
    for i, a in enumerate(available) :
        if a not in ls :
            d[key] = a
            del available[i]
            del verifiedRules[key]
            break

myTicket = [int(x) for x in content[1][1].split(',')]
total = 1
for i in range(6) :
    print(f"departure {i} : {myTicket[d[i]]} ")
    total *= myTicket[d[i]]
print(f"Multiplication : {total}")