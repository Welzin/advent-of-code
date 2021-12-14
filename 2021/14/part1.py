#! /usr/bin/env python3

def step(polymer, transformations) :
    res = ""
    for i in range(len(polymer) - 1) :
        res += polymer[i] + transformations[polymer[i] + polymer[i + 1]]
    res += polymer[-1]
    return res

lines = [line.strip() for line in open("input")]
polymer = lines[0] 
transfos = dict([line.split(' -> ') for line in lines[2:]])

for _ in range(10) :
    polymer = step(polymer, transfos)

count = {c:polymer.count(c) for c in set(polymer)}
count = [v for _, v in sorted(count.items(), key = lambda x : x[1])]
print(count[-1] - count[0])