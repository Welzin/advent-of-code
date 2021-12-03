#! /usr/bin/env python3

entries = [line.strip() for line in open("input")]
print(g:=sum(2**(len(entries[0]) - i - 1)*int(max(lst:=[entry[i] for entry in entries], key=lst.count)) for i in range(len(entries[0])))*((2**(len(entries[0]))-1)^g))