#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

grid = open(filename).read().splitlines()
width = len(grid[0])
height = len(grid)


def count_trees(grid, dx, dy):
    x, y = 0, 0
    count = 0
    while y < height:
        square = grid[y][x % width]
        if square == "#":
            count += 1
        x += dx
        y += dy
    return count


# star 1
print(count_trees(grid, 3, 1))

# star 2
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
product = 1
for i in slopes:
    product *= count_trees(grid, *i)
print(product)
