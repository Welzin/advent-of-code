#! /usr/bin/python3

def isValid(passport) :
    needed = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']
    requiredFields = 0
    for required in needed :
        if required in current :
            requiredFields += 1

    return requiredFields == len(needed)

fd = open("input", "r")

count = 0
current = ""
for line in fd.readlines() :

    if line == "\n" :
        count += isValid(current)
        current = ""
    
    current += " " + line.strip()
    

count += isValid(current)
print(count)