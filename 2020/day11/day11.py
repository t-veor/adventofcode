#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

initial_layout = open(filename).read().splitlines()
width = len(initial_layout[0])
height = len(initial_layout)


def first_in_dir(layout, start, dir, limit=-1):
    width, height = len(layout[0]), len(layout)
    x, y = start
    dx, dy = dir
    x += dx
    y += dy
    steps = 0
    while 0 <= x < width and 0 <= y < height and steps != limit:
        if layout[y][x] == "L":
            return x, y
        steps += 1
        x += dx
        y += dy
    return None


def seat_graph(layout, limit=-1):
    dirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    width, height = len(layout[0]), len(layout)
    index_mapping = {}
    index_graph = {}
    index = 0
    for y in range(height):
        for x in range(width):
            if layout[y][x] == "L":
                index_mapping[x, y] = index
                index_graph[x, y] = []
                for dir in dirs:
                    f = first_in_dir(layout, (x, y), dir, limit)
                    if f:
                        index_graph[x, y].append(f)
                index += 1

    graph = {}
    for i in index_graph:
        graph[index_mapping[i]] = [index_mapping[j] for j in index_graph[i]]
    return graph, [False] * index


def step(graph, state, free_count):
    new_state = []
    for i, s in enumerate(state):
        if s:
            if sum(1 for j in graph[i] if state[j]) >= free_count:
                new_state.append(False)
            else:
                new_state.append(True)
        else:
            if all(not state[j] for j in graph[i]):
                new_state.append(True)
            else:
                new_state.append(False)
    return new_state


def print_state(initial_layout, state):
    index = 0
    for y in range(len(initial_layout)):
        for x in range(len(initial_layout[0])):
            if initial_layout[y][x] == "L":
                if state[index]:
                    print("#", end="")
                else:
                    print("L", end="")
                index += 1
            else:
                print(".", end="")
        print()


# star 1
graph, state = seat_graph(initial_layout, limit=1)
while True:
    prev_state = state
    state = step(graph, state, 4)
    if state == prev_state:
        break
print(sum(1 for i in state if i))

# star 2
graph, state = seat_graph(initial_layout)
prev_state = None
while True:
    prev_state = state
    state = step(graph, state, 5)
    if state == prev_state:
        break
print(sum(1 for i in state if i))
