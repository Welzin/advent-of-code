#! /usr/bin/env python3

def solve_str(N, i=0) :
    V = int(N[i:i+3], 2)
    T = int(N[i+3:i+6], 2)
    i += 6

    if T == 4 :
        while N[i] == '1' :
            i += 5
        return V, i+5
    else :
        I, Vs = N[i], V
        i += 1

        if I == '0' :
            L = int(N[i:i+15], 2)
            i += 15
            end = i + L

            while i < end :
                Vc, i = solve_str(N, i)
                Vs += Vc
        else :
            L = int(N[i:i+11], 2)
            i += 11

            for _ in range(L) :
                Vc, i = solve_str(N, i)
                Vs += Vc

        return Vs, i

print(solve_str(bin(int(open("input").readline(), 16))[2:].zfill(4))[0])