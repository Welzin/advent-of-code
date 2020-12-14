#! /usr/bin/env python3

import itertools

with open("input", "r") as fd :
    content = [x.split('=') for x in fd.read().strip().split('\n')]

values = dict()
mask   = content[0][1].strip()

for k, v in content :
    k, v = k.strip(), v.strip()
    if k == 'mask' :
        mask = v
    else :
        k, v = k[k.find('[')+1:-1], int(v)

        val = [x for x in bin(int(k))[2:]]
        while len(val) != 36 : val.insert(0, '0')

        for i in range(len(mask)) :
            if mask[i] == '0' :
                continue
            val[i] = mask[i]

        combinations = list(itertools.product([0, 1], repeat=val.count('X')))
        copy = val[:]
        for c in combinations :
            for bit in c :
                copy[copy.index('X')] = str(bit)
            values[int(''.join(copy), base=2)] = v
            copy = val[:]

print(f"Sum : {sum(values.values())}")