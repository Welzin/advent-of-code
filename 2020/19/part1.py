#! /usr/bin/env python3

import re, collections
from typing import List, Dict, T

def readRules(rules: List[str]) -> Dict[int, T] :
    res = {}
    for rule in rules :
        match = re.match(r"(\d+): ([\d |]+)", rule)
        if match != None :
            rule, do = match.groups()
            res[int(rule)] = [list(map(int, x.strip().split(' '))) for x in do.split('|')]
        else :
            char = rule.split(':')
            res[int(char[0])] = char[1].replace('"', '').strip()
    return res

def constructRegex(rules: Dict[int, T], rule: int) -> str :
    regex = ""
    for rls in rules[rule] :
        for r in rls :
            if isinstance(rules[r], str) :
                regex += rules[r]
            else :
                regex += "(" + constructRegex(rules, r) + ")"
        regex += "|"
    regex = regex[:-1]
        
    return regex

with open("input", "r") as fd :
    content = [x.split('\n') for x in fd.read().split('\n\n')]

regex = "^" + constructRegex(readRules(content[0]), 0) + "$"
print(f"Messages matching rule 0: {sum(re.match(regex, x) != None for x in content[1])}")