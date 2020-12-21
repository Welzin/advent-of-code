#! /usr/bin/env python3

import re
from typing import List

def intersection(l1: List[str], l2: List[str]) -> List[str] :
    return [v for v in l1 if v in l2] 

with open("input", "r") as fd :
    content = fd.read().strip().split('\n')

ingredients, allergens = {}, {}

for line in content :
    i, a = line[:line.find('(')], line[line.find("contains")+8:-1]
    ings, alls = re.findall(r"([^ ^(^)^,]+)", i), re.findall(r"([^ ^(^)^,]+)", a)
    for ing in ings :
        if ing in ingredients :
            ingredients[ing] += 1
        else :
            ingredients[ing] = 1
    for al in alls :
        if al in allergens :
            allergens[al] = intersection(allergens[al], ings)
        else :
            allergens[al] = ings

allergens = {k: allergens[k] for k in sorted(allergens, key=lambda k: len(allergens[k]))}
verifiedAllergens = []

for k, v in allergens.items() :
    last = ''
    for a in v :
        if a not in verifiedAllergens :
            if last != '' :
                verifiedAllergens.remove(last)
            verifiedAllergens.append(a)
            allergens[k], last = a, a
allergens = {k: allergens[k] for k in sorted(allergens)}

print(f"Ingredients containing allergens: {','.join(allergens.values())}")