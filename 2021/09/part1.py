#! /usr/bin/env python3

def lowest_point(n, i, j, numbers) :
    neighbors = filter(lambda tup: 0 <= tup[0] < len(numbers) and 0 <= tup[1] < len(numbers[0]), [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)])
    return all(n < numbers[x][y] for x, y in neighbors)

numbers = [list(map(int, [c for c in line.strip()])) for line in open("input")]
print(sum(sum(numbers[i][j] + 1 for j in range(len(numbers[0])) if lowest_point(numbers[i][j], i, j, numbers)) for i in range(len(numbers))))