#! /usr/bin/python3

import os

fd = open("input", "r")

entries = []

for line in fd.readlines() :
    entries.append(int(line))

# Naive - O(n^3) solution :
for i in range(len(entries)) :
    for j in range(i + 1, len(entries)) :
        for k in range(j + 1, len(entries)) :
            if entries[i] + entries[j] + entries[k] == 2020 :
                print("O(n^3) : {}".format(entries[i] * entries[j] * entries[k]))
                break

# O(n^2) solution. 
# See also : https://en.wikipedia.org/wiki/3SUM
entries.sort()

for i in range(len(entries) - 1) :
    a = entries[i]
    st = i + 1
    ed = len(entries) - 1
    while st < ed :
        b = entries[st]
        c = entries[ed]
        if a + b + c == 2020 :
            print("O(n^2) : {}".format(a * b * c))
            break
        elif a + b + c > 2020 :
            ed -= 1
        else :
            st += 1