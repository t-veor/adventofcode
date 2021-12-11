#!/usr/bin/env python3
from collections import deque
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

original_grid = [[int(j) for j in i] for i in open(filename).read().strip().splitlines()]


def adjacent(x, y, width, height):
    for i in range(max(0, x - 1), min(x + 2, width)):
        for j in range(max(0, y - 1), min(y + 2, height)):
            if (i, j) != (x, y):
                yield i, j


def print_grid(grid):
    for l in grid:
        print("".join(str(i) for i in l))


def step(grid):
    width, height = len(grid[0]), len(grid)
    will_flash = deque()
    flashed = set()

    for y in range(height):
        for x in range(width):
            grid[y][x] += 1
            if grid[y][x] > 9:
                will_flash.append((x, y))

    while will_flash:
        x, y = will_flash.popleft()
        if (x, y) in flashed:
            continue

        flashed.add((x, y))
        for x2, y2 in adjacent(x, y, width, height):
            grid[y2][x2] += 1
            if grid[y2][x2] > 9:
                will_flash.append((x2, y2))

    for x, y in flashed:
        grid[y][x] = 0

    return len(flashed)

# star 1
grid = [i[:] for i in original_grid]
flashes = 0
for i in range(100):
    flashes += step(grid)
print(flashes)

# star 2
grid = [i[:] for i in original_grid]
width, height = len(grid[0]), len(grid)
i = 0
while True:
    i += 1
    if step(grid) == width * height:
        break
print(i)
