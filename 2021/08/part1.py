#! /usr/bin/env python3

numbers = [line.split("|")[1].split() for line in open("input")]
print(sum(sum(len(n) == 2 or len(n) == 3 or len(n) == 4 or len(n) == 7 for n in l) for l in numbers))