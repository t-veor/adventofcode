#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

initial_config = open(filename).read().splitlines()

initial_state = set()
for y, row in enumerate(initial_config):
    for x, c in enumerate(row):
        if c == "#":
            initial_state.add((x, y, 0))


def neighbours(pos):
    x, y, z = pos
    for x_ in range(x - 1, x + 2):
        for y_ in range(y - 1, y + 2):
            for z_ in range(z - 1, z + 2):
                if (x_, y_, z_) != (x, y, z):
                    yield (x_, y_, z_)


def neighbours_4(pos):
    x, y, z, w = pos
    for x_ in range(x - 1, x + 2):
        for y_ in range(y - 1, y + 2):
            for z_ in range(z - 1, z + 2):
                for w_ in range(w - 1, w + 2):
                    if (x_, y_, z_, w_) != pos:
                        yield (x_, y_, z_, w_)


def step(state):
    min_x, max_x = min(i[0] for i in state), max(i[0] for i in state)
    min_y, max_y = min(i[1] for i in state), max(i[1] for i in state)
    min_z, max_z = min(i[2] for i in state), max(i[2] for i in state)

    new_state = set()
    for x in range(min_x - 1, max_x + 2):
        for y in range(min_y - 1, max_y + 2):
            for z in range(min_z - 1, max_z + 2):
                neighbour_count = 0
                for n in neighbours((x, y, z)):
                    if n in state:
                        neighbour_count += 1
                        if neighbour_count > 3:
                            break
                if (x, y, z) in state:
                    if 2 <= neighbour_count <= 3:
                        new_state.add((x, y, z))
                else:
                    if neighbour_count == 3:
                        new_state.add((x, y, z))
    return new_state


def step_4(state):
    min_x, max_x = min(i[0] for i in state), max(i[0] for i in state)
    min_y, max_y = min(i[1] for i in state), max(i[1] for i in state)
    min_z, max_z = min(i[2] for i in state), max(i[2] for i in state)
    min_w, max_w = min(i[3] for i in state), max(i[3] for i in state)

    new_state = set()
    for x in range(min_x - 1, max_x + 2):
        for y in range(min_y - 1, max_y + 2):
            for z in range(min_z - 1, max_z + 2):
                for w in range(min_w - 1, max_w + 2):
                    pos = (x, y, z, w)
                    neighbour_count = 0
                    for n in neighbours_4(pos):
                        if n in state:
                            neighbour_count += 1
                            if neighbour_count > 3:
                                break
                    if pos in state:
                        if 2 <= neighbour_count <= 3:
                            new_state.add(pos)
                    else:
                        if neighbour_count == 3:
                            new_state.add(pos)
    return new_state


def print_state(state):
    min_x, max_x = min(i[0] for i in state), max(i[0] for i in state)
    min_y, max_y = min(i[1] for i in state), max(i[1] for i in state)
    min_z, max_z = min(i[2] for i in state), max(i[2] for i in state)

    for z in range(min_z, max_z + 1):
        print("z={}".format(z))
        for y in range(min_y, max_y + 1):
            for x in range(min_x, max_x + 1):
                if (x, y, z) in state:
                    print("#", end="")
                else:
                    print(".", end="")
            print()
        print()


# star 1
state = initial_state
for _ in range(6):
    state = step(state)
print(len(state))

# star 2
state = set((*i, 0) for i in initial_state)
for _ in range(6):
    state = step_4(state)
print(len(state))
