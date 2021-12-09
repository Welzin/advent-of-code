#! /usr/bin/env python3

def analyse_pattern(ls) :
    corresponds = {2:1,7:8,4:4,3:7}
    dct = {corresponds[len(i)]:i for i in ls if len(i) in [2, 3, 4, 7]}
    for i in range(10) :
        s = len(ls[i])
        if s == 6 : #0 6 9
            if sum(ls[i][j] in dct[4] for j in range(s)) == 3 :
                if sum(ls[i][j] in dct[1] for j in range(s)) == 2 :
                    dct[0] = ls[i]
                else :
                    dct[6] = ls[i]
            else :
                dct[9] = ls[i]
        elif s == 5 : #2 3 5
            if sum(ls[i][j] in dct[7] for j in range(s)) == 3 :
                dct[3] = ls[i]
            elif sum(ls[i][j] in dct[4] for j in range(s)) == 3 :
                dct[5] = ls[i]
            else :
                dct[2] = ls[i]

    return dct

def get_number(dct, n) :
    return int([i for i in range(10) if len(n) == len(dct[i]) and all(c in dct[i] for c in n)][0])

numbers = [tuple(map(lambda x:x.split(), line.split("|"))) for line in open("input")]
print(sum(sum(10**(len(signals)-i-1) * get_number(analyse_pattern(patterns), signals[i]) for i in range(len(signals))) for patterns, signals in numbers))