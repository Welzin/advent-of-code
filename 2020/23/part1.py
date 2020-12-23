#! /usr/bin/env python3

from typing import List, Tuple

class CupGame:
    data: List[int]
    head: int

    def __init__(self, ls: List[int]) -> None :
        self.data = ls
        self.head = 0

    def run(self, i) -> None :
        save = self.step1()
        dest = self.step2(save)
        self.step3(dest, save)
        self.step4()

    def step1(self) -> Tuple[int, int, int] :
        st, md, ed = (self.head + 1) % len(self.data), (self.head + 2) % len(self.data), (self.head + 3) % len(self.data)
        save = (self.data[st], self.data[md], self.data[ed])
        return save

    def step2(self, save: Tuple[int, int, int]) -> int :
        val = self.data[self.head] - 1
        for i in save : self.data.remove(i)
        while val not in self.data and val >= min(self.data) : val -= 1
        return self.data.index(val) if val in self.data else self.data.index(max(self.data))

    def step3(self, dest: int, saved: Tuple[int, int, int]) -> None :
        if dest < self.head : self.head += 3
        self.data[dest+1:dest+1] = list(saved)

    def step4(self) -> None :
        self.head += 1
        if self.head >= len(self.data) : self.head = 0

    def answer(self) -> None :
        st = self.data.index(1)
        for i in range(st + 1, len(self.data)) :
            print(self.data[i], end='')
        for i in range(st) :
            print(self.data[i], end='')
        print('\n', end='')

    def __repr__(self) -> str :
        return "CupGame(" + ', '.join([str(x) for x in self.data]) + ")"


with open("input", "r") as fd :
    game = CupGame([int(w) for w in fd.read().strip()]) 

for i in range(100) : game.run(i)

game.answer()