#! /usr/bin/env python3

entries = [int(n) for n in open("input")]
print(sum([entries[i-1] < entries[i] for i in range(1, len(entries))]))