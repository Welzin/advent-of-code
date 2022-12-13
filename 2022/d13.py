#! /usr/bin/env pypy3.9

from functools import cmp_to_key

def is_order_ok(L1, L2) :
    i, j = 0, 0
    while i < len(L1) and j < len(L2) :
        if isinstance(L1[i], int) and isinstance(L2[j], int) :
            if L1[i] < L2[j] : return -1
            if L1[i] > L2[j] : return 1
        if isinstance(L1[i], list) and isinstance(L2[j], list) :
            ret = is_order_ok(L1[i], L2[j])
            if ret != 0 : return ret
        if isinstance(L1[i], int) and isinstance(L2[j], list) :
            ret = is_order_ok([L1[i]], L2[j])
            if ret != 0 : return ret
        if isinstance(L1[i], list) and isinstance(L2[j], int) :
            ret = is_order_ok(L1[i], [L2[j]])
            if ret != 0 : return ret
        i += 1
        j += 1

    if i == len(L1) and j < len(L2) : return -1
    if i < len(L1) and j == len(L2) : return 1
    return 0
            
part1 = 0
part2 = [[[2]], [[6]]]
try :
    i = 1
    while True :
        tmp = input()
        if len(tmp.strip()) == 0 : continue
        
        L1 = eval(tmp)
        L2 = eval(input())

        part2.append(L1)
        part2.append(L2)
        
        ret = is_order_ok(L1, L2)
        if ret == -1 : 
            part1 += i

        i += 1
except EOFError :
    pass

print(part1)

part2 = sorted(part2, key=cmp_to_key(is_order_ok))
print((part2.index([[2]])+1) * (part2.index([[6]])+1))