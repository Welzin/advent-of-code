#! /usr/bin/env python3

# This solution uses Chinese Remainder.
# I found that the problem wanted us to find the least common multiple
# of a range of numbers with remainders.
# The first link google gave me when typing this was a forum, which talked
# about the Chinese Remainder. It looked like it fit the problem, so I went
# on wikipedia and applied the algorithm for a generic Chinese Remainder, 
# which uses the Extended Euclidean Algorithm. Everything is explained here :
# https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Computation

def euc(a, b) :
    if b == 0 :
        return (a, 1, 0)
    else :
        d, u, v = euc(b, a % b)
        return (d, v, u - ((a // b) * v))

with open("input", "r") as fd :
    content = fd.read().strip().split('\n')
    bus     = [(i, int(x)) for i, x in enumerate(content[1].split(',')) if x != 'x']

n = 1
for d, b in bus : n *= b

total = 0
for d, b in bus : 
    ni = n // b
    _, u, v = euc(b, ni)
    ei = v * ni
    total += ei * (b - d)

print(total % n)