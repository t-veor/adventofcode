#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

lines = []
for l in open(filename).read().splitlines():
    start, end = l.split(" -> ")
    start = tuple(int(i) for i in start.split(","))
    end = tuple(int(i) for i in end.split(","))
    lines.append((start, end))

# star 1
squares = {}
for (x1, y1), (x2, y2) in lines:
    if x1 == x2:
        if y2 < y1:
            y1, y2 = y2, y1
        for y in range(y1, y2 + 1):
            squares[x1, y] = squares.get((x1, y), 0) + 1
    elif y1 == y2:
        if x2 < x1:
            x1, x2 = x2, x1
        for x in range(x1, x2 + 1):
            squares[x, y1] = squares.get((x, y1), 0) + 1
    else:
        pass

print(len([None for i in squares.values() if i >= 2]))

# star 2
def sign(x):
    if x == 0:
        return 0
    elif x > 0:
        return 1
    else:
        return -1

squares = {}
for (x1, y1), (x2, y2) in lines:
    x, y = x1, y1
    dx = sign(x2 - x1)
    dy = sign(y2 - y1)
    count = max(abs(x2 - x1), abs(y2 - y1))

    for _ in range(count + 1):
        squares[x, y] = squares.get((x, y), 0) + 1
        x += dx
        y += dy

print(len([None for i in squares.values() if i >= 2]))
