#! /usr/bin/env python3

import functools

def evaluate(expr: str) -> int :
    res, curr, i = [], 0, 0
    while i < len(expr) :
        c = expr[i]
        if c != ' ' :
            if c == '*' :
                res.append(curr)
                curr = 0
            elif c == '(' :
                p_v, p_e = 1, i + 1
                while p_v > 0:
                    if expr[p_e] == ')':
                        p_v -= 1
                    elif expr[p_e] == '(':
                        p_v += 1
                    p_e += 1
                p_e -= 1
                curr += evaluate(expr[i+1:p_e])
                i = p_e
            elif c != '+' :
                curr += int(c)
        i += 1
    res.append(curr)
    return functools.reduce((lambda x, y: x * y), res)

with open("input", "r") as fd :
    expressions = fd.read().split('\n')

print(f"Sum: {sum([evaluate(expr) for expr in expressions])}")