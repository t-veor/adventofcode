#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

board = {}
keys = {}
doors = {}
for y, l in enumerate(open(filename).read().splitlines()):
    for x, c in enumerate(l):
        board[x, y] = c
        if c.islower():
            keys[x, y] = c
        elif c.isupper():
            doors[x, y] = c


def cardinals(pos):
    x, y = pos
    yield (x + 1, y)
    yield (x - 1, y)
    yield (x, y + 1)
    yield (x, y - 1)


def adjacent(board, pos):
    for i in cardinals(pos):
        if board.get(i, "#") != "#":
            yield i


def paths_between(board, start, end):
    visited = set()
    found_paths = []

    def recurse(pos, path_length, encountered_doors):
        if pos == end:
            found_paths.append((path_length, tuple(encountered_doors)))

        visited.add(pos)
        for i in adjacent(board, pos):
            if i not in visited:
                new_doors = encountered_doors
                if board[i].isupper():
                    new_doors = encountered_doors + [board[i]]
                recurse(i, path_length + 1, new_doors)
        visited.remove(pos)

    recurse(start, 0, [])

    # discard duplicate paths with the same cost and encountered_doors
    found_paths = list(set(found_paths))

    return found_paths


for i in keys:
    for j in keys:
        if i != j:
            print(i, j)
            print(board[i], "->", board[j], paths_between(board, i, j))
