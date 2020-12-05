#! /usr/bin/env python3

# Improved previous code with cleaner rule checking :
#   - Improved RegEx with the solutions from here : https://www.reddit.com/r/adventofcode/comments/k6e8sw/2020_day_04_solutions/
#   - Improved file reading process
# Original clean version : https://www.reddit.com/user/Gramineae/

import re

def isValid(passport) :
    needed = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    for required in needed :
        if required not in passport :
            return False

    return True

fd = open("input", "r")
info = fd.read()
passports = info.rstrip().split("\n\n")
count = 0

for passport in passports :
    info = passport.replace("\n", " ")
    if isValid(info) :
        byr = re.search(r"byr:(19[2-9][0-9]|200[0-2])", info) != None
        iyr = re.search(r"iyr:(201[0-9]|2020)", info) != None
        eyr = re.search(r"eyr:(20[2][0-9]|2030)", info) != None
        hgt = re.search(r"hgt:(1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-3]in)", info) != None
        hcl = re.search(r"hcl:#[0-9a-f]{6}", info) != None
        ecl = re.search(r"ecl:(amb|blu|brn|gry|grn|hzl|oth)", info) != None
        pid = re.search(r"pid:[0-9]{9}", info) != None
        if byr and iyr and eyr and hgt and hcl and ecl and pid:
            count += 1
    
print(count)