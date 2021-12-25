#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

grid = open(filename).read().strip().splitlines()
width, height = len(grid[0]), len(grid)

horizontal_cucumbers, vertical_cucumbers = set(), set()
for y, row in enumerate(grid):
    for x, c in enumerate(row):
        if c == ">":
            horizontal_cucumbers.add((x, y))
        elif c == "v":
            vertical_cucumbers.add((x, y))

board = ((width, height), horizontal_cucumbers, vertical_cucumbers)


def print_board(board):
    (width, height), horizontal_cucumbers, vertical_cucumbers = board
    for y in range(height):
        for x in range(width):
            if (x, y) in horizontal_cucumbers:
                print(">", end="")
            elif (x, y) in vertical_cucumbers:
                print("v", end="")
            else:
                print(".", end="")
        print()
    print()


def step(board):
    (width, height), horizontal_cucumbers, vertical_cucumbers = board
    new_horizontal_cucumbers, new_vertical_cucumbers = set(), set()

    moved = False
    for (x, y) in horizontal_cucumbers:
        target = ((x + 1) % width, y)
        if target in horizontal_cucumbers or target in vertical_cucumbers:
            new_horizontal_cucumbers.add((x, y))
        else:
            moved = True
            new_horizontal_cucumbers.add(target)

    for (x, y) in vertical_cucumbers:
        target = (x, (y + 1) % height)
        if target in new_horizontal_cucumbers or target in vertical_cucumbers:
            new_vertical_cucumbers.add((x, y))
        else:
            moved = True
            new_vertical_cucumbers.add(target)

    return moved, ((width, height), new_horizontal_cucumbers, new_vertical_cucumbers)

i = 0
while True:
    moved, board = step(board)
    i += 1
    if not moved:
        print(i)
        break



