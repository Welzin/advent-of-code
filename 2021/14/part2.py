#! /usr/bin/env python3

def step(count, polymer, transformations) :
    for k, v in polymer.copy().items() :
        polymer[k] -= v
        char_res = transformations[k]
        count[char_res] = insert(count, char_res, v)
        keys = [k[0] + char_res, char_res + k[1]]
        for key in keys : 
            polymer[key] = insert(polymer, key, v)
    return (polymer, count)

def insert(d, key, value) :
    if key in d : return d[key] + value
    else : return value

def init(polymer) :
    dct = {}
    for i in range(len(polymer) - 1) :
        pair = polymer[i] + polymer[i + 1]
        if pair in dct : dct[pair] += 1
        else : dct[pair] = 1 
    return dct

lines = [line.strip() for line in open("input")]
polymer = init(lines[0]) 
transfos = dict([line.split(' -> ') for line in lines[2:]])
count = {c:lines[0].count(c) for c in lines[0]}

for i in range(40) :
    polymer, count = step(count, polymer, transfos)

count = [v for _, v in sorted(count.items(), key = lambda x : x[1])]
print(count[-1] - count[0])