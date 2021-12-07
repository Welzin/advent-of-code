#! /usr/bin/env python3

alignment = list(map(int, open("input").readline().split(",")))
print(min(sum((abs(alignment[j] - i)*(abs(alignment[j] - i)+1))//2 for j in range(len(alignment))) for i in range(max(alignment))))