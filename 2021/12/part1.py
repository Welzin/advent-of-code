#! /usr/bin/env python3

def add_verticle(a, b) :
    global verticles
    if a in verticles : verticles[a].append(b)
    else : verticles[a] = [b]
    return verticles

def all_paths_util(u, path = []) :
    global verticles

    if u in path and not u.upper() == u : return []

    path.append(u)
    
    if u == 'end':
        return [path]

    paths = []
    for nextCave in verticles[u]:
        paths += all_paths_util(nextCave, path[:])
    return paths

verticles = {}

for line in open("input") :
    a, b = line.strip().split('-')
    if b != 'start' :
        verticles = add_verticle(a, b)
        verticles = add_verticle(b, a)
    else :
        verticles = add_verticle(b, a)

print(len(all_paths_util('start')))