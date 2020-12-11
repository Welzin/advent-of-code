#! /usr/bin/env python3

with open("input", "r") as fd :
    instructions = [x.split(' ') for x in fd.read().split('\n')]

acc  = 0
done = [False] * len(instructions)
i    = 0

while 0 <= i < len(instructions) and not done[i] :
    done[i] = True
    instruction, value = instructions[i]

    if instruction == "acc" :
        acc += int(value)
    if instruction == "jmp" :
        i += int(value)
    else :
        i += 1

print(f"Accumulator: {acc}")