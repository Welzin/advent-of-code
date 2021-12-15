#! /usr/bin/env python3

# Approximation working on easy examples
def solve_dynamic(chitons) :
    solve = [[0] * len(chitons[0]) for _ in range(len(chitons))] 
    
    for i in range(1, len(chitons[0])) :
        solve[0][i] = solve[0][i-1] + chitons[0][i]
    for i in range(1, len(chitons)) :
        solve[i][0] = solve[i-1][0] + chitons[i][0]

    for i in range(1, len(chitons)) :
        for j in range(1, len(chitons[0])) :
            solve[j][i] = min(solve[j][i-1] + chitons[j][i], solve[j-1][i] + chitons[j][i])
        for j in range(1, len(chitons)) :
            solve[i][j] = min(solve[i-1][j] + chitons[i][j], solve[i][j-1] + chitons[i][j])
    
    return solve[-1][-1]


chitons = [[int(c) for c in line.strip()] for line in open("input")]
rows, cols = len(chitons), len(chitons[0])

for i in range(4) :
    left = [chiton[i*cols:(i*cols)+cols] for chiton in chitons]
    for j in range(rows) :
        for k in range(cols) :
            s = (left[j][k] + 1) % 10
            chitons[j].append(s if s != 0 else 1)
for i in range(rows, 5*rows) :
    row = []
    for j in range(5*cols) :
        s = (chitons[i-rows][j] + 1) % 10
        row.append(s if s != 0 else 1)
    chitons.append(row)

print(solve_dynamic(chitons))