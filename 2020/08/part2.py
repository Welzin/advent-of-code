#! /usr/bin/env python3

def run(instructions) :
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

    return (acc, i != len(instructions))

def swap(instruction) :
    if instruction == "jmp" :
        return "nop"
    if instruction == "nop" :
        return "jmp"
    return instruction

with open("input", "r") as fd :
    instructions = [x.split(' ') for x in fd.read().split('\n')]

for i in range(len(instructions)) :
    instructions[i][0] = swap(instructions[i][0])
    out, isLoop = run(instructions)
    instructions[i][0] = swap(instructions[i][0])
    if not isLoop :
        print(f"Accumulator: {out}")
        break