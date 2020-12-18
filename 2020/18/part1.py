#! /usr/bin/env python3

def evaluate(expr: str) -> int :
    res, op, i = 0, '+', 0
    while i < len(expr) :
        c = expr[i]
        if c != ' ' :
            if c == '*' or c == '+' :
                op = c
            elif c == '(' :
                p_v, p_e = 1, i + 1
                while p_v > 0:
                    if expr[p_e] == ')':
                        p_v -= 1
                    elif expr[p_e] == '(':
                        p_v += 1
                    p_e += 1
                p_e -= 1
                if op == '+':
                    res += evaluate(expr[i+1:p_e])
                else:
                    res *= evaluate(expr[i+1:p_e])
                i = p_e
            elif c == ')' :
                pass
            else :
                if op == '+' :
                    res += int(c)
                else :
                    res *= int(c)
        i += 1
    return res

with open("input", "r") as fd :
    expressions = fd.read().split('\n')

print(f"Sum: {sum([evaluate(expr) for expr in expressions])}")