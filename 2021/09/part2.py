#! /usr/bin/env python3

import math

def get_basin(i, j, numbers):
    if 0 <= i < len(numbers) and 0 <= j < len(numbers[i]) and numbers[i][j] != 9:
        numbers[i][j] = 9
        return 1 + get_basin(i - 1, j, numbers) + get_basin(i + 1, j, numbers) + get_basin(i, j - 1, numbers) + get_basin(i, j + 1, numbers) 
    return 0

numbers = [list(map(int, [c for c in line.strip()])) for line in open("input")]
print(math.prod(sorted([get_basin(i, j, numbers) for j in range(len(numbers[0])) for i in range(len(numbers))], reverse=True)[:3]))