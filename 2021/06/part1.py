#! /usr/bin/env python3

def process(population, days) :
    total = [population.count(i) for i in range(days)]
    for day in range(days - 7) : total[day + 7] += total[day] + total[day - 2] * (day >= 2)
    return sum(total) + sum(population.count(i) for i in range(days))

print(process(list(map(int, open("input").readline().split(","))), 80))