#! /usr/bin/env python3

def add_verticle(a, b) :
    global verticles
    if a in verticles : verticles[a].append(b)
    else : verticles[a] = [b]
    return verticles

def all_paths_util(u, path = [], canDoubleVisit = True) :
    global verticles

    if u in path and not u.upper() == u :
        if not(canDoubleVisit) :
            return []
        canDoubleVisit = False

    path.append(u)
    
    if u == 'end':
        return [path]

    paths = []
    for nextCave in verticles[u]:
        paths += all_paths_util(nextCave, path[::], canDoubleVisit)
    return paths

verticles = {'end':[]}

for line in open("input") :
    a, b = line.strip().split('-')
    if b != 'start' and a != 'end' :
        verticles = add_verticle(a, b)
    if a != 'start' and b != 'end' :
        verticles = add_verticle(b, a)

print(len(all_paths_util('start')))