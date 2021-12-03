#! /usr/bin/env python3

entries = [line.strip() for line in open("input")];t1 = [entries[:]];t2 = [entries[:]]

print(int([x for x in [(t1.append(list(filter(lambda b:b[i] == ('1' if [entry[i] for entry in t1[-1]].count('1') >= [entry[i] for entry in t1[-1]].count('0') else '0'), t1[-1]))), t1[-1])[1] for i in range(len(t1[-1][0]))] if x][-1][0], 2)*int([x for x in [(t2.append(list(filter(lambda b:b[i] == ('0' if [entry[i] for entry in t2[-1]].count('0') <= [entry[i] for entry in t2[-1]].count('1') else '1'), t2[-1]))), t2[-1])[1] for i in range(len(t2[-1][0]))] if x][-1][0], 2))