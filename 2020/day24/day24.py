#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


def lex(directions):
    result = []
    index = 0
    while index < len(directions):
        if directions[index] in "ns":
            result.append(directions[index:index+2])
            index += 2
        else:
            result.append(directions[index])
            index += 1
    return result


tile_directions = [lex(i) for i in open(filename).read().splitlines()]


DELTA_MAP = {
    "e": (1, 0),
    "se": (1, -1),
    "sw": (0, -1),
    "w": (-1, 0),
    "nw": (-1, 1),
    "ne": (0, 1),
}


# star 1
flipped = set()
for dirs in tile_directions:
    x, y = 0, 0
    for dir in dirs:
        dx, dy = DELTA_MAP[dir]
        x += dx
        y += dy
    if (x, y) in flipped:
        flipped.remove((x, y))
    else:
        flipped.add((x, y))
print(len(flipped))


# star 2
def step(state):
    need_to_update = set()
    for pos in state:
        x, y = pos
        need_to_update.add(pos)
        for dx, dy in DELTA_MAP.values():
            need_to_update.add((x + dx, y + dy))

    new_state = set()
    for pos in need_to_update:
        x, y = pos
        neighbour_count = 0
        for dx, dy in DELTA_MAP.values():
            if (x + dx, y + dy) in state:
                neighbour_count += 1

        if pos in state:
            if neighbour_count > 0 and neighbour_count <= 2:
                new_state.add(pos)
        else:
            if neighbour_count == 2:
                new_state.add(pos)

    return new_state

state = flipped
for _ in range(100):
    state = step(state)
print(len(state))
