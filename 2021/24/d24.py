#! /usr/bin/env python3

import re
from time import time

def fun(div, add_x, add_y, w, z) :
    if (z%26)+add_x != w :
        return (26*(z//div))+w+add_y
    else :
        return z//div


MONAD = open("24.txt").read().split('\n') 
div, add_x, add_y = -1, -1, -1
to_apply = [] 

for i in range(len(MONAD)) :
    if 'div' in MONAD[i] : 
        div = int(re.findall(r"-?\d+", MONAD[i])[0])
    add = re.match(r"add x (-?\d+)", MONAD[i])
    if add is not None :
        add_x = int(add.group(1))
    if i > 1 and MONAD[i-1] == 'add y w' :
        add_y = int(re.findall(r"-?\d+", MONAD[i])[0])
        to_apply.append((div, add_x, add_y))

stack = {0: [0, 0]}
st = time()
k = 0

for div, add_x, add_y in to_apply :
    newStack = {}
    print(f"Processing program number {k}... time elapsed since start : {time() - st}s, {len(stack)} to treat.")
    k += 1 
    for z, [min, max] in stack.items() :
        for i in range(1, 10) :
            new_z = fun(div, add_x, add_y, i, z)
            if new_z in newStack :
                mn, mx = newStack[new_z]
                if min*10+i < mn : mn = min*10+i
                if max*10+i > mx : mx = max*10+i
                newStack[new_z] = [mn, mx]
            else :
                newStack[new_z] = [min*10+i, max*10+i]
    stack = newStack

print(f"Results: {stack[0]}, {time() - st}s")