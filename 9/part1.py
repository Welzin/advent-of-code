#! /usr/bin/env python3

def isSum(data, number) :
    return any(True for i in range(len(data)) if number - data[i] in data[i + 1:])

with open("input", "r") as fd :
    numbers = [int(x) for x in fd.read().split('\n')]

for i in range(25, len(numbers)) :
    if not isSum(numbers[i - 25:i], numbers[i]) :
        print(numbers[i])
        break