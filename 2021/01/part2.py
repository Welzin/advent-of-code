#! /usr/bin/env python3

entries = [int(n) for n in open("input")]
print(sum([entries[i] < entries[i+3] for i in range(len(entries)-3)]))