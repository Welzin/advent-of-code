#! /usr/bin/env python3

from math import ceil, floor

class Node :
    def __init__(self, value = None, leftChild = None, rightChild = None) :
        self.__v = value
        self.__lc = leftChild
        self.__rc = rightChild

    def leftChild(self) : return self.__lc
    def rightChild(self) : return self.__rc
    def value(self) : return self.__v

    def reduce(self) :
        while self.__canExplode() or self.__canSplit() :
            if self.__canExplode() : self.__explode()
            elif self.__canSplit() : self.__split()

    def add(self, other) :
        return Node(None, self, other)

    def magnitude(self) :
        if self.__v is not None : return self.__v

        l, r = 0, 0
        if self.__lc : l = 3*self.__lc.magnitude()
        if self.__rc : r = 2*self.__rc.magnitude()
        return l + r
 
    def __explode(self, depth = 0) :
        vl, vr = None, None
        if depth == 4 :
            if self.__lc and self.__rc :
                vl, vr = self.__lc.__v, self.__rc.__v
                self.__v, self.__lc, self.__rc = 0, None, None 
                return vl, vr
        else :
            if self.__lc :
                res = self.__lc.__explode(depth + 1)
                if res == True : return True
                vl, vr = res
                if vr and self.__rc :
                    self.__rc.__add(vr, 'L')
                    vr = None
                    if vl is None : return True

            if vl is None and vr is None and self.__rc :
                res = self.__rc.__explode(depth + 1)
                if res == True : return True
                vl, vr = res
                if vl and self.__lc :
                    self.__lc.__add(vl, 'R')
                    vl = None
                    if vr is None : return True

        return vl, vr

    def __split(self) :
        if self.__v is not None and self.__v >= 10 :
            self.__lc = Node(floor(self.__v/2))
            self.__rc = Node(ceil(self.__v/2))
            self.__v = None
            return True
        else :
            split = False
            if self.__lc : split = self.__lc.__split()
            if not(split) and self.__rc : split = self.__rc.__split()
            return split

    def __canSplit(self) :
        if self.__v is not None and self.__v >= 10 : return True
        ls, rs = False, False
        if self.__lc  : ls = self.__lc.__canSplit() 
        if self.__rc : rs = self.__rc.__canSplit()
        return ls or rs

    def __canExplode(self, depth = 0) :
        if depth == 4 and self.__lc and self.__rc : return True
        ls, rs = False, False
        if self.__lc : ls = self.__lc.__canExplode(depth + 1) 
        if self.__rc : rs = self.__rc.__canExplode(depth + 1)
        return ls or rs

    def __add(self, v, comingFrom) :
        if self.__v is not None :
            self.__v += v
        else :
            if comingFrom == 'R' :
                if self.__rc : self.__rc.__add(v, comingFrom)
                elif self.__lc : self.__lc.__add(v, comingFrom)
            else :
                if self.__lc : self.__lc.__add(v, comingFrom)
                elif self.__rc : self.__rc.__add(v, comingFrom)

def makeTree(str) :
    if str[0] == '[' :
        comaIndex, interPairs = 1, 0
        while not(interPairs == 0 and str[comaIndex] == ',') :
            if str[comaIndex] == '[' : interPairs += 1
            if str[comaIndex] == ']' : interPairs -= 1
            comaIndex += 1
        return Node(None, makeTree(str[1:comaIndex]), makeTree(str[comaIndex+1:-1]))
    else :
        return Node(int(str[0]))

def displayTree(node) :
    if node is None : return ''
    if node.value() is not None : return str(node.value())

    vl = displayTree(node.leftChild())
    vr = displayTree(node.rightChild())
    return '[' + vl + ',' + vr + ']'

lines = open("input").readlines()

max_magnitude = 0
for line in lines :
    root = makeTree(line)
    for line2 in lines :
        test = root.add(makeTree(line2))
        test.reduce()
        if test.magnitude() > max_magnitude : max_magnitude = test.magnitude()
    for line2 in lines :
        root = makeTree(line2).add(makeTree(line))
        root.reduce()
        if root.magnitude() > max_magnitude : max_magnitude = root.magnitude()
print(max_magnitude)