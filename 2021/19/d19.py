#! /usr/bin/env python3

from time import time

def dist(v1, v2) :
    x, y, z = v1 
    a, b, c = v2 
    return (x-a)**2 + (y-b)**2 + (z-c)**2

def create_distances(scanner) :
    return [dist(scanner[i], scanner[j]) for i in range(len(scanner)) for j in range(i + 1, len(scanner))]

def get_possible_matches(distances) :
    return {i:[j for j in range(i+1, len(distances)) if len(set(distances[i]).intersection(distances[j])) >= 66] for i in range(len(distances))}

def get_transformed_data(matched, scanner) :
    perms = {
        (0, 1, 2) : [(1, 1, 1), (1, -1, -1), (-1, -1, 1), (-1, 1, -1)],
        (0, 2, 1) : [(1, -1, 1), (1, 1, -1), (-1, -1, -1), (-1, 1, 1)],
        (1, 0, 2) : [(-1, 1, 1), (1, 1, -1), (1, -1, 1), (-1, -1, -1)],
        (1, 2, 0) : [(1, 1, 1), (-1, 1, -1), (1, -1, -1), (-1, -1, 1)],
        (2, 0, 1) : [(1, 1, 1), (-1, 1, -1), (1, -1, -1), (-1, -1, 1)],
        (2, 1, 0) : [(-1, 1, 1), (1, 1, -1), (1, -1, 1), (-1, -1, -1)]
    }
    oriented = [[tuple(v[p[i]] * m[i] for i in range(len(v))) for v in scanner] for p, ms in perms.items() for m in ms]
    for orient in oriented :
        for v in orient :
            for start in matched :
                diff = tuple(-(v[i] - start[i]) for i in range(len(v)))
                t = [tuple(v2[i] + diff[i] for i in range(len(v2))) for v2 in orient]
                if common(t, matched) >= 12 : return (t, diff)

    return None

def common(s1, s2) :
    return sum(int(v in s2) for v in s1)

def part1(scanners) :
    print(len(set().union(*scanners)))

def part2(pos) :
    print(max(abs(xb-xa)+abs(yb-ya)+abs(zb-za) for xb, yb, zb in pos for xa, ya, za in pos))

def solve(scanners) :
    distances = [create_distances(scanner) for scanner in scanners]
    matches = get_possible_matches(distances)
    matched = [0]
    scanners_pos = [None] * len(scanners)

    while len(matched) != len(scanners) :
        for i, toMatch in matches.items() :
            print(i)
            if i in matched :
                for j in toMatch :
                    if j not in matched :
                        scanners[j], scanners_pos[j] = get_transformed_data(scanners[i], scanners[j])
                        matched.append(j)
            else :
                for j in toMatch :
                    if j in matched :
                        scanners[i], scanners_pos[i] = get_transformed_data(scanners[j], scanners[i])
                        matched.append(i)
                        break
        print(f"Matched: {len(matched)}")

    scanners_pos[0] = (0, 0, 0)
    part1(scanners)
    part2(scanners_pos)

scanners = []
for scanner in [[line for line in lines.split('\n')] for lines in open("input").read().split('\n\n')] :
    s = []
    for elem in scanner :
        if '---' in elem : continue
        s.append(tuple(map(int, elem.split(','))))
    scanners.append(s)

st = time()
solve(scanners)
print(f"{time() - st}s")