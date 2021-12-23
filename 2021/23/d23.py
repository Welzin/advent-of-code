#! /usr/bin/env python3

import re, collections

def completed(game_map) :
    return game_map[(1, 2)] == game_map[(2, 2)] == game_map[(3, 2)] == game_map[(4, 2)] == 'A' and \
           game_map[(1, 4)] == game_map[(2, 4)] == game_map[(3, 4)] == game_map[(4, 4)] == 'B' and \
           game_map[(1, 6)] == game_map[(2, 6)] == game_map[(3, 6)] == game_map[(4, 6)] == 'C' and \
           game_map[(1, 8)] == game_map[(2, 8)] == game_map[(3, 8)] == game_map[(4, 8)] == 'D' 

def cost(game_map, src, dest) :
    scores = {'A':1, 'B':10, 'C':100, 'D':1000}
    letter = game_map[src]
    return (abs(src[1]-dest[1]) + src[0] + dest[0])*scores[letter]

def can_move(game_map, src, dest) :
    if src[0] == 2 and game_map[(1, src[1])] is not None : return False
    st, ed = min(src[1], dest[1]), max(src[1], dest[1])
    return all([(0, y) == src or game_map[(0, y)] is None for y in range(st, ed+1) if (st, ed) != src])

def possible_moves(game_map) :
    rooms = {'A':2, 'B':4, 'C': 6, 'D': 8}
    moves = []
    # 1 - From hallway to its room or room to another room
    hallway = [(x, y, v) for (x, y), v in game_map.items() if v is not None and x == 0]
    roomsToHallway = [(x, y, v) for (x, y), v in game_map.items() if v is not None and x != 0 and game_map[(1, y)] != game_map[(2, y)]]
    for x, y, letter in hallway + roomsToHallway :
        room = rooms[letter]
        if game_map[(4, room)] is None :
            if can_move(game_map, (x, y), (2, room)) : moves.append(((x, y), (2, room)))
        elif (4, room) != (x, y) and game_map[(3, room)] == letter and game_map[(2, room)] is None :
            if can_move(game_map, (x, y), (1, room)) : moves.append(((x, y), (1, room)))
        elif (3, room) != (x, y) and game_map[(2, room)] == letter and game_map[(1, room)] is None :
            if can_move(game_map, (x, y), (1, room)) : moves.append(((x, y), (1, room)))
    # 2 - From room to hallway
    availableHallwayNodes = [(x, y) for (x, y), v in game_map.items() if v is None and x == 0 and y not in rooms.values()]
    for x, y, letter in roomsToHallway :
        if (x, y) != (4, rooms[letter]) and (x, y) != (3, rooms[letter]) and (x, y) != (2, rooms[letter]) :
            for a, b in availableHallwayNodes :
                if can_move(game_map, (x, y), (a, b)) : moves.append(((x, y), (a, b)))
    return moves

def print_game_map(game_map) :
    gm = [['.' for _ in range(11)] for _ in range(3)]
    for (x, y), v in game_map.items() :
        if v is not None :
            gm[x][y] = v
    print('\n'.join([''.join(line) for line in gm]))
    print('\n')

def solve_bruteforce(game_map) :
    min_score = 2**32
    score = 0
    state = []
    possible_scores = []
    while len(state) > 0 or len(possible_scores) == 0 :
        moves = possible_moves(game_map)
        if score > min_score or len(moves) == 0 or completed(game_map) :
            if completed(game_map) and score < min_score :
                min_score = score
                print(score)
            while len(state[-1][0]) == 0 or state[-1][2] > min_score : 
                state.pop()
                if len(state) == 0 : return min_score
            moves = state[-1][0]
            game_map = state[-1][1]
            score    = state[-1][2]

        src, dest = moves.pop(0)
        if len(moves) > 0 :
            state.append([moves, game_map.copy(), score])
        score += cost(game_map, src, dest)
        game_map[src], game_map[dest] = None, game_map[src]

    return min_score

def cost2(src, dest, weight) :
    return (abs(src[1]-dest[1]) + src[0] + dest[0])*weight

tot = 0
for line in open("23.txt").readlines() :
    xs, ys, xd, yd, weight = map(int, re.findall('\d+', line))
    src = (xs, ys)
    dst = (xd, yd)
    tot += cost2(src, dst, weight)
print(tot)