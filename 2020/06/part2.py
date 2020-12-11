#! /usr/bin/env python3

content = open("input", "r").read().rstrip().split('\n\n')
sum = 0

for entry in content :
    # Add the length of the intersection of all sets of a group
    sum += len(set.intersection(*[set(answer) for answer in entry.split('\n')]))

print("Sum : ", sum)