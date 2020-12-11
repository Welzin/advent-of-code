#! /usr/bin/env python3

content = [x.replace('\n', '') for x in open("input", "r").read().rstrip().split('\n\n')]
# Sum the length of the different characters in each group answers
total = sum([len(set(answer)) for answer in content])

print("Sum : ", total)