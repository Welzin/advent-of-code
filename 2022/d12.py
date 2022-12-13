#! /usr/bin/env pypy3.9

import heapq
from math import inf
from collections import defaultdict

def dijkstra(n, src, succ):
    dist = [inf] * n
    dist[src] = 0
    prev = [None] * n
    prev[src] = src
    queue = [(0, src)]
    while queue:
        u = heapq.heappop(queue)[1]
        for v, w in succ[u]:
            alt = dist[u] + w
            if dist[v] > alt:
                dist[v] = alt
                prev[v] = u
                heapq.heappush(queue, (dist[v], v))
    return dist, prev

grid = []
P    = []
st, ed = [], None

try :
    while True :
        grid.append(input())
        for j in range(len(grid[-1])) :
            i = len(grid) - 1
            P.append((i, j))
            if grid[-1][j] == 'S' :
                st.append(len(P) - 1)
                grid[-1] = grid[-1].replace('S', 'a')
            if grid[-1][j] == 'E' :
                ed = len(P) - 1
                grid[-1] = grid[-1].replace('E', 'z')
            if grid[-1][j] == 'a' :
                st.append(len(P) - 1)
except EOFError :
    pass

def addNeighbor(G, src, dst, P) :
    G[P.index(src)].append((P.index(dst), 1))

def isNeighbor(src, dst) :
    x, y = src
    a, b = dst
    return ord(grid[a][b]) <= ord(grid[x][y]) + 1

G = defaultdict(list)

# Top row
for i in range(1, len(grid[0])) :
    if isNeighbor((0, i), (0, i - 1)) :
        addNeighbor(G, (0, i), (0, i - 1), P)
    if isNeighbor((0, i - 1), (0, i)) :
        addNeighbor(G, (0, i - 1), (0, i), P)
# Left col
for i in range(1, len(grid)) :
    if isNeighbor((i - 1, 0), (i, 0)) :
        addNeighbor(G, (i - 1, 0), (i, 0), P)
    if isNeighbor((i, 0), (i - 1, 0)) :
        addNeighbor(G, (i, 0), (i - 1, 0), P)

# Others
for i in range(1, len(grid)) :
    for j in range(1, len(grid[i])) :
        # Left
        if isNeighbor((i - 1, j), (i, j)) :
            addNeighbor(G, (i - 1, j), (i, j), P)
        if isNeighbor((i, j), (i - 1, j)) :
            addNeighbor(G, (i, j), (i - 1, j), P)
        # Top
        if isNeighbor((i, j - 1), (i, j)) :
            addNeighbor(G, (i, j - 1), (i, j), P)
        if isNeighbor((i, j), (i, j - 1)) :
            addNeighbor(G, (i, j), (i, j - 1), P)

dists = []
for s in st :
    dist, _ = dijkstra(len(P), s, G)
    dists.append(dist[ed])

# Part 1
print(dists[0])
print(min(dists))
        
