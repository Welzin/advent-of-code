#! /usr/bin/env python3

import math

def solve(N, i=0) :
    T = int(N[i+3:i+6], 2)
    i += 6

    if T == 4 :
        value = ''
        while N[i] == '1' :
            value += N[i+1:i+5]
            i += 5
        return int(value+N[i+1:i+5],2), i+5
    else :
        I, Vs = N[i], []
        i += 1

        if I == '0' :
            L = int(N[i:i+15], 2)
            i += 15
            end = i + L

            while i < end :
                Vc, i = solve(N, i)
                Vs.append(Vc)
        else :
            L = int(N[i:i+11], 2)
            i += 11

            for _ in range(L) :
                Vc, i = solve(N, i)
                Vs.append(Vc)

        value = 0
        if T == 0 : value = sum(Vs)
        elif T == 1 : value = math.prod(Vs)
        elif T == 2 : value = min(Vs)
        elif T == 3 : value = max(Vs)
        elif T == 5 : value = int(Vs[0] > Vs[1])
        elif T == 6 : value = int(Vs[0] < Vs[1])
        elif T == 7 : value = int(Vs[0] == Vs[1])
        return value, i

print(solve(bin(int(open("input").readline(), 16))[2:].zfill(4))[0])