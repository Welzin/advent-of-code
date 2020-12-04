#! /usr/bin/python3

import re

def isValid(passport) :
    match = re.findall("([^ ]+):([^ ]+)", passport)

    if len(match) == 7 and "cid" in passport :
        return False

    if len(match) >= 7 :
        for kv in match :
            rule = kv[0]
            val  = kv[1]

            if rule == "byr" :
                if int(val) < 1920 or int(val) > 2002 : return False
            elif rule == "iyr" :
                if int(val) < 2010 or int(val) > 2020 : return False
            elif rule == "eyr" :
                if int(val) < 2020 or int(val) > 2030 : return False
            elif rule == "hgt" :
                try :
                    if val.find("cm") != -1 :
                        if (int(val[0:3]) < 150 or int(val[0:3]) > 193) : return False
                    elif val.find("in") != -1 :
                        if (int(val[0:2]) < 59 or int(val[0:2]) > 76) : return False
                    else :
                        return False
                except ValueError :
                    return False
            elif rule == "hcl" :
                pattern = re.compile(r"#[A-Za-z0-9]{6}")
                if len(val) > 7 or pattern.match(val) == None : return False
            elif rule == "ecl" :
                pattern = re.compile(r"amb|blu|brn|gry|grn|hzl|oth")
                if len(val) > 3 or pattern.match(val) == None : return False
            elif rule == "pid" :
                pattern = re.compile(r"[0-9]{9}")
                if len(val) > 9 or pattern.match(val) == None : return False
    
        return True
    else :
        return False

fd = open("input", "r")

count = 0
current = ""
for line in fd.readlines() :
    if line == "\n" :
        count += int(isValid(current))
        current = ""
    
    current += " " + line.strip()
    

count += int(isValid(current))
print(count)