#! /usr/bin/env python3

def loop(target: int, subjectNumber: int, encrypt: bool) -> int :
    num, count = 1, 0
    while (not encrypt and num != target) or (encrypt and count < target) :
        num *= subjectNumber
        num %= 20201227
        count += 1
    return count, num

cardPkey, doorPkey = 14222596, 4057428
sizeCard, sizeDoor = loop(cardPkey, 7, False)[0], loop(doorPkey, 7, False)[0]
print(f"Encryption key: {loop(sizeDoor, cardPkey, True)[1]}, {loop(sizeCard, doorPkey, True)[1]}")