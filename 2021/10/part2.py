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
    ')' : 1,
    ']' : 2,
    '}' : 3,
    '>' : 4,
}
closure = {
    '[' : ']',
    '{' : '}',
    '(' : ')',
    '<' : '>'
}
closures = []
for line in open("input") :
    queue = []
    try :
        for c in line.strip() :
            queue = process(queue, c)
    except RuntimeError as r :
        continue
    close = []
    while len(queue) > 0 :
        current = queue.pop()
        close.append(closure[current])
    tot = 0
    for c in close :
        tot *= 5
        tot += values[c]
    closures.append(tot)

closures.sort()
print(closures, len(closure))
print(closures[len(closures) // 2])