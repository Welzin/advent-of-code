#! /usr/bin/python3

import re

def isValid(passport) :
    needed = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']
    for required in needed :
        if required not in passport :
            return False

    return True

fd = open("input", "r")
info = fd.read()
passports = info.rstrip().split('\n\n')
count = 0

for passport in passports :
    info = passport.replace('\n', ' ')
    if isValid(info) :
        byr = re.search(r'byr:(19[2-9][0-9]|200[0-2])', info)
        iyr = re.search(r'iyr:(201[0-9]|2020)', info)
        eyr = re.search(r'eyr:(20[2][0-9]|2030)', info)
        hgt = re.search(r'hgt:(1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-3]in)', info)
        hcl = re.search(r'hcl:#[0-9a-f]{6}', info)
        ecl = re.search(r'ecl:(amb|blu|brn|gry|grn|hzl|oth)', info)
        pid = re.search(r'pid:[0-9]{9}', info)
        if byr and iyr and eyr and hgt and hcl and ecl and pid:
            count += 1
    
print(count)