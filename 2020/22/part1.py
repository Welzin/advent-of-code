#! /usr/bin/env python3

from typing import List, T

class Queue:
    data: List[T]

    def __init__(self, ls: List[T]) -> None :
        self.data = ls

    def queue(self, element: T) -> None :
        self.data.append(element)

    def dequeue(self) -> T :
        return self.data.pop(0)

    def peek(self) -> T :
        return self.data[0]

    def size(self) -> int :
        return len(self.data)

    def __repr__(self) -> str :
        return "Queue(" + ', '.join([str(x) for x in self.data]) + ")"


with open("input", "r") as fd :
    p1, p2 = (Queue([int(w) for w in x.split('\n')[1:]]) for x in fd.read().strip().split('\n\n'))

while p1.size() > 0 and p2.size() > 0 :
    cardP1, cardP2 = p1.dequeue(), p2.dequeue()
    if cardP1 > cardP2 :
        p1.queue(cardP1)
        p1.queue(cardP2)
    else :
        p2.queue(cardP2)
        p2.queue(cardP1)

if p1.size() > p2.size() : winner = p1
else : winner = p2

print(f"Winner's score : {sum([x * (winner.size() - i) for i, x in enumerate(winner.data)])}")