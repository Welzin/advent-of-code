#! /usr/bin/env python3

with open("input", "r") as fd :
    adapters = [int(x) for x in fd.read().strip().split('\n')]

# Best arrangement
adapters.sort()
adapters.append(max(adapters) + 3)

ans = { 0: 1 }

for a in adapters :
    # ways to go from 0 to current adapter. i.e. : [1, 4, 5, 6, 7]
    # we have { 1: 1, 4: 1, 5: 1, 6: 2, 7: 4 } ==> [1, 4, 5, 6, 7] ; [1, 4, 5, 7] ; [1, 4, 6, 7] ; [1, 4, 7]
    # the only way to have 5 is to have 4, but 6 can be obtained from 4 & 5, and 7 from 4 * 1, 5 * 1 or 6 * 2.
    ans[a] = ans.get(a - 1, 0) + ans.get(a - 2, 0) + ans.get(a - 3, 0)

print(ans[adapters[-1]])