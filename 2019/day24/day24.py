#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

GRID_WIDTH = 5
GRID_SIZE = 25

initial_state = 0
initial_state_set = set()
for i, j in enumerate("".join(open(filename).read().split())):
    if j == "#":
        initial_state |= 1 << i
        initial_state_set.add((0, i))


def adjacent(pos):
    if pos % GRID_WIDTH != 0:
        yield pos - 1
    if pos % GRID_WIDTH != GRID_WIDTH - 1:
        yield pos + 1
    if pos - GRID_WIDTH >= 0:
        yield pos - GRID_WIDTH
    if pos + GRID_WIDTH < GRID_SIZE:
        yield pos + GRID_WIDTH


def step(prev):
    new = 0
    for i in range(GRID_SIZE):
        num_adjacent = 0
        for j in adjacent(i):
            if prev & (1 << j):
                num_adjacent += 1

        if prev & (1 << i) and num_adjacent == 1:
            new |= 1 << i
        elif not (prev & (1 << i)) and 1 <= num_adjacent <= 2:
            new |= 1 << i
    return new


def print_state(state):
    for i in range(0, GRID_SIZE, GRID_WIDTH):
        for j in range(i, i + GRID_WIDTH):
            if state & (1 << j):
                print("#", end="")
            else:
                print(".", end="")
        print()


# star 1
seen_states = set()
state = initial_state
while True:
    seen_states.add(state)
    state = step(state)
    if state in seen_states:
        print(state)
        break


def recursive_adj(level, pos):
    # fuck it just hardcode the numbers
    # left
    if pos == 13:
        for i in range(4, 25, 5):
            yield level + 1, i
    elif pos % 5 == 0:
        yield level - 1, 11
    else:
        yield level, pos - 1

    # right
    if pos == 11:
        for i in range(0, 21, 5):
            yield level + 1, i
    elif pos % 5 == 4:
        yield level - 1, 13
    else:
        yield level, pos + 1

    # up
    if pos == 17:
        for i in range(20, 25):
            yield level + 1, i
    elif pos < 5:
        yield level - 1, 7
    else:
        yield level, pos - 5

    # down
    if pos == 7:
        for i in range(5):
            yield level + 1, i
    elif pos >= 20:
        yield level - 1, 17
    else:
        yield level, pos + 5


def recursive_step(prev_state):
    new_state = set()
    levels = list(i for i, _ in prev_state)
    min_level = min(levels)
    max_level = max(levels)

    for level in range(min_level - 1, max_level + 2):
        for j in range(25):
            if j == 12:
                continue

            num_adjacent = 0
            for adj in recursive_adj(level, j):
                if adj in prev_state:
                    num_adjacent += 1

            if (level, j) in prev_state and num_adjacent == 1:
                new_state.add((level, j))
            elif (level, j) not in prev_state and 1 <= num_adjacent <= 2:
                new_state.add((level, j))

    return new_state

# star 2
state = initial_state_set
for _ in range(200):
    state = recursive_step(state)
print(len(state))
