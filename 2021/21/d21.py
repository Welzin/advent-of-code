#! /usr/bin/env python3

from copy import deepcopy

def move(from_, dice, score) :
    for _ in range(3) :
        dice += 1
        from_ += dice
        while from_ > 10 : from_ -= 10
    return from_, dice, score + from_

def part1(p1, p2) :
    dice, scorep1, scorep2 = 0,0,0

    while scorep1 < 1000 and scorep2 < 1000 :
        p1, dice, scorep1 = move(p1, dice, scorep1)
        if scorep1 < 1000 : p2, dice, scorep2 = move(p2, dice, scorep2)

    if scorep2 < scorep1 : print(scorep2 * dice)
    else : print(scorep1 * dice)

def finished(universes) :
    return all(k[2] == 21 or k[3] == 21 for k, _ in universes.items()) and True

def scores(universes) :
    return sum(v for k, v in universes.items() if k[2] == 21), sum(v for k, v in universes.items() if k[3] == 21)

def step(universes, moves, player = 1) :
    copy = deepcopy(universes)
    for key, val in copy.items() :
        p1, p2, s1, s2 = key

        if s1 != 21 and s2 != 21 :
            for k, v in moves.items() :
                new_pos = key[player-1] + k 
                if new_pos > 10 : new_pos -= 10
                new_score = key[player+1] + new_pos
                if new_score > 21 : new_score = 21
                tup = [p1, p2, s1, s2]
                tup[player-1] = new_pos 
                tup[player+1] = new_score
                tup = tuple(tup)
                if tup in universes : universes[tup] += v*val
                else : universes[tup] = v*val
            if key in universes : 
                universes[key] -= val 
                if universes[key] <= 0 : del universes[key]
    return universes

def part2(p1, p2) :
    universes = {(p1, p2, 0, 0): 1}
    moves = { 3: 1, 4: 3, 5: 6, 6: 7, 7: 6, 8: 3, 9: 1 }
    assert sum(moves.values()) == 27

    while not(finished(universes)) :
        universes = step(universes, moves, 1)
        universes = step(universes, moves, 2)

    print(max(scores(universes)))
    
#p1, p2 = 4, 8
p1, p2 = 8, 3

part1(p1, p2)
part2(p1, p2)