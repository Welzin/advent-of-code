#! /usr/bin/env python3

alignment = list(map(int, open("input").readline().split(",")))
print(min([sum(abs(alignment[j] - alignment[i]) for j in range(len(alignment))) for i in range(len(alignment))]))