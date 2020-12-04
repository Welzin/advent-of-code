#! /usr/bin/python3

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
    count += isValid(info)
    
print(count)