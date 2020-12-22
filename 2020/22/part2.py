#! /usr/bin/env python3

import copy
from typing import List, T, Tuple

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

    def __eq__(self, oth) -> bool :
        return self.data == oth.data

def recursionGame(deck1: Queue, deck2: Queue) -> Tuple[Queue, Queue, int] :
    configurations = []
    while deck1.size() > 0 and deck2.size() > 0 :
        if (deck1, deck2) in configurations :
            return (deck1, deck2, 1)

        configurations.append((copy.deepcopy(deck1), copy.deepcopy(deck2)))

        cardP1, cardP2 = deck1.dequeue(), deck2.dequeue()

        if cardP1 <= deck1.size() and cardP2 <= deck2.size() :
            _, _1, winner = recursionGame(Queue(deck1.data[:cardP1]), Queue(deck2.data[:cardP2]))
            if winner == 1 :
                deck1.queue(cardP1)
                deck1.queue(cardP2)
            else :
                deck2.queue(cardP2)
                deck2.queue(cardP1)
        else :
            if cardP1 > cardP2 :
                deck1.queue(cardP1)
                deck1.queue(cardP2)
            else :
                deck2.queue(cardP2)
                deck2.queue(cardP1)

    if deck1.size() > deck2.size() : return (deck1, Queue([]), 1)
    else : return (Queue([]), deck2, 2)


with open("input", "r") as fd :
    p1, p2 = (Queue([int(w) for w in x.split('\n')[1:]]) for x in fd.read().strip().split('\n\n'))

deck1, deck2, winner = recursionGame(p1, p2)
if winner == 1 : winner = deck1
else : winner = deck2
print(f"Winner's score : {sum([x * (winner.size() - i) for i, x in enumerate(winner.data)])}")