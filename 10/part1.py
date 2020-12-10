#! /usr/bin/env python3

with open("input", "r") as fd :
    adapters = [int(x) for x in fd.read().strip().split('\n')]
    adapters.append(0)
    adapters.append(max(adapters) + 3)

adapters.sort()

differences = [adapters[i + 1] - adapters[i] for i in range(len(adapters) - 1)]

print(differences.count(1) * differences.count(3))