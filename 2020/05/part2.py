#! /usr/bin/env python3

def getId(line) :
    # Convert to binary
    row = int(line[:7].replace('B', '1').replace('F', '0'), 2)
    col = int(line[7:].replace('R', '1').replace('L', '0'), 2)

    return row * 8 + col

# O(nlog(n)) solution
ids = [getId(line) for line in open("input", "r").readlines()]
ids.sort()

for i in range(len(ids) - 1) :
    if ids[i + 1] - ids[i] == 2 :
        print("Your seat id (O(nlog(n))) :", ids[i] + 1)
        break

# O(n) solution
# EDIT 12-06-2020 : O(n) solution was easily done. There are 2^10 available seats. 
# With that, we can instantiate an array of booleans to know which seats are occupied :
seats = [False] * 2**10
for line in open("input", "r").readlines() :
    seats[getId(line)] = True

for i in range(len(seats)) :
    if not seats[i] and seats[i - 1] and seats[i + 1] :
        print("Your seat id (O(n)) :", i)
        break 