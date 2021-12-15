#! /usr/bin/env python3

def make_graph(cols, rows) :
    graph = {}
    for i in range(rows) :
        for j in range(cols) :
            for x, y in [(i-1, j), (i+1, j), (i, j-1), (i, j+1)] :
                if 0 <= x < rows and 0 <= y < cols :
                    if (i, j) in graph : graph[(i, j)].append((x, y))
                    else : graph[(i, j)] = [(x, y)] 
    return graph

def dijkstra(G, s, p) :
    F = [s]
    D = {u:2**32 for u in G.keys()}
    D[s] = 0
    while len(F) > 0 :
        test = {s:D[s] for s in F}
        u = min(test, key=test.get)
        F.remove(u)
        for x, y in G[u] :
            v = (x, y)
            if D[u] + p[x][y] < D[v] :
                D[v] = D[u] + p[x][y]
                F.append(v)
    return D[list(D.keys())[-1]]


chitons = [[int(c) for c in line.strip()] for line in open("input")]
print(dijkstra(make_graph(len(chitons[0]), len(chitons)), (0, 0), chitons))