#! /usr/bin/env python3

with open("input", "r") as fd :
    content = [x.split('=') for x in fd.read().strip().split('\n')]

values = dict()
mask   = content[0][1].strip()

for k, v in content :
    k, v = k.strip(), v.strip()
    if k == 'mask' :
        mask = v
    else :
        k = k[k.find('[')+1:-1]

        val = [x for x in bin(int(v))[2:]]
        while len(val) != 36 : val.insert(0, '0')

        for i in range(len(mask)) :
            if mask[i] == 'X' :
                continue
            val[i] = mask[i]
        
        values[int(k)] = int(''.join(val), base=2)

print(f"Sum : {sum(values.values())}")