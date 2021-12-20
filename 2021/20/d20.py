#! /usr/bin/env python3

def calculate_point(enhancement, light_pixels, x, y, t, xmin, xmax, ymin, ymax) :
    binary = ''.join('1' if (x+i, y+j) in light_pixels or (t & 1 != 0 and not(xmin <= x+i <= xmax and ymin <= y+j <= ymax)) else '0' for i in [-1, 0, 1] for j in [-1, 0, 1])
    return enhancement[int(binary, 2)] == '#'

def run(enhancement, light_pixels, turns) :
    for i in range(turns) :
        xmin, xmax, ymin, ymax = min(x for x, _ in light_pixels), max(x for x, _ in light_pixels), min(y for _, y in light_pixels), max(y for _, y in light_pixels)
        light_pixels = {(x,y):1 for x in range(xmin-1, xmax+2) for y in range(ymin-1,ymax+2) if calculate_point(enhancement, light_pixels, x, y, i, xmin, xmax, ymin, ymax)}
    return len(light_pixels)

enhancement, image = open("input").read().split('\n\n')

image = image.split('\n')
light_pixels = {(i, j):1 for i in range(len(image)) for j in range(len(image[i])) if image[i][j] == '#'}

print(run(enhancement, light_pixels, 2)) # Part 1
print(run(enhancement, light_pixels, 50)) # Part 2