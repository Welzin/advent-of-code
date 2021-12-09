#! /usr/bin/env python3

numbers = [line.split("|")[1].split() for line in open("input")]
print(sum(sum(len(n) in [2, 3, 4, 7] for n in l) for l in numbers))