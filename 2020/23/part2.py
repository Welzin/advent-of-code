#! /usr/bin/env python3

from typing import List, Tuple, Dict, T

# Linked List
class Node : 
    val : int
    nxt : T

    def __init__(self, val) :
        self.val = val
        self.nxt = None

class CupGame :
    head: Node
    ndes: List[Node]

    def __init__(self, ls: List[int]) -> None :
        self.ndes = {}
        prev = None
        for x in ls :
            curr = Node(x)
            if prev is not None :
                prev.nxt = curr
            prev = curr
            self.ndes[x] = curr
        self.head = self.ndes[ls[0]]
        prev.nxt = self.head

    def run(self, i) -> None :
        if(i % 1000000 == 0) : print(f"{i // 1000000}/9")
        a, b, c = self.step1()
        dst = self.step2(a, b, c)
        self.step3(a, b, c, dst)

    def step1(self) -> Tuple[Node, Node, Node] :
        a = self.head.nxt
        b = a.nxt
        c = b.nxt
        self.head.nxt = c.nxt
        return (a, b, c)

    def step2(self, a: Node, b: Node, c: Node) -> Node :
        val = self.head.val - 1 or 1000000
        while val in (a.val, b.val, c.val) :
            val = val - 1 or 1000000
        return self.ndes[val]

    def step3(self, a: Node, b: Node, c: Node, dst: Node) -> None :
        c.nxt = dst.nxt
        dst.nxt = a
        self.head = self.head.nxt

    def answer(self) -> None :
        node = self.ndes[1]
        print(f"{node.nxt.val} * {node.nxt.nxt.val} : {node.nxt.val * node.nxt.nxt.val}")

with open("input", "r") as fd :
    base = [int(w) for w in fd.read().strip()]
    for i in range(max(base) + 1, 1000001) :
        base.append(i)
    game = CupGame(base)

for i in range(10000000) : game.run(i)

game.answer()