#! /usr/bin/env python3

def process(queue, char) :
    entry = {
        ']' : '[',
        '}' : '{',
        ')' : '(',
        '>' : '<'
    }
    if char in '[{(<' :
        return queue + [char]
    else :
        if queue.pop() != entry[char] : 
            raise RuntimeError(char)
    return queue

values = {
    ')' : 3,
    ']' : 57,
    '}' : 1197,
    '>' : 25137
}
sum = 0
for line in open("input") :
    queue = []
    try :
        for c in line.strip() :
            queue = process(queue, c)
    except RuntimeError as r :
        sum += values[r.args[0]]
print(sum)