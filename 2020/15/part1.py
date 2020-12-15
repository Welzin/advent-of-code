#! /usr/bin/env python3

with open("input", "r") as fd :
    startNumbers = [int(x) for x in fd.read().strip().split(',')]

numbers = {num: i + 1 for i, num in enumerate(startNumbers[:-1])}
last    = startNumbers[-1]
for i in range(len(startNumbers), 2020) :
    numbers[last], last = i, (i - numbers[last]) if last in numbers else 0

print(last)