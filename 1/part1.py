#! /usr/bin/env python3

import os

fd = open("input", "r")

entries = []

for line in fd.readlines() :
    entries.append(int(line))

# Naive - O(n^2) solution :
for i in range(len(entries)) :
    for j in range(i + 1, len(entries)) :
        if entries[i] + entries[j] == 2020 :
            print("O(n^2) : {}".format(entries[i] * entries[j]))
            break

# Python's sort algorithm has O(nlog(n)) complexity. The following has O(nlog(n)) complexity. 
# See also : https://stackoverflow.com/questions/9656789/find-2-numbers-in-an-unsorted-array-equal-to-a-given-sum
entries.sort()
st = 0
ed = len(entries) - 1
found = False

while not found :
    sum = entries[st] + entries[ed]
    if sum == 2020 :
        found = True
    elif sum < 2020 :
        st += 1
    else :
        ed -= 1

print("O(nlog(n)) : {}".format(entries[st] * entries[ed]))