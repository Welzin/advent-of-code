#! /usr/bin/env pypy3.9

def parse(in_) : return in_[0], int(in_[1])
def move(dir) :
    if dir == 'R' : return 0, 1
    elif dir == 'L' : return 0, -1
    elif dir == 'U' : return 1, 0
    elif dir == 'D' : return -1, 0

visited = set([(0, 0)])
v2      = set([(0, 0)])
hx, hy  = 0, 0
tx, ty  = 0, 0
tail    = [(0, 0) for _ in range(10)]

try :
    while True :
        d, n = parse(input().split())
        for _ in range(n) :
            x, y = move(d)
            hx, hy = hx + x, hy + y
            
            if abs(hx - tx) == 2 and abs(hy - ty) == 0 : tx += (hx - tx)//2
            elif abs(hy - ty) == 2 and abs(hx - tx) == 0 : ty += (hy - ty)//2
            elif abs(hx - tx) == 2 and abs(hy - ty) == 1 : tx, ty = tx+(hx - tx)//2, hy
            elif abs(hy - ty) == 2 and abs(hx - tx) == 1 : tx, ty = hx, ty+(hy - ty)//2
            visited.add((tx, ty))
            
            tail[0] = (tail[0][0] + x, tail[0][1] + y)
            for i in range(1, len(tail)) :
                xh, yh = tail[i-1]
                xt, yt = tail[i]
                if abs(xh - xt) == 2 and abs(yh - yt) == 0 : xt += (xh - xt)//2
                elif abs(yh - yt) == 2 and abs(xh - xt) == 0 : yt += (yh - yt)//2
                elif abs(xh - xt) == 2 and abs(yh - yt) == 1 : xt, yt = xt+(xh - xt)//2, yh
                elif abs(yh - yt) == 2 and abs(xh - xt) == 1 : xt, yt = xh, yt+(yh - yt)//2
                elif abs(yh - yt) == 2 and abs(xh - xt) == 2 : xt, yt = xt+(xh-xt)//2, yt+(yh-yt)//2
                tail[i] = (xt, yt)

            v2.add(tail[-1])

except EOFError :
    pass

print(len(visited))
print(len(v2))
