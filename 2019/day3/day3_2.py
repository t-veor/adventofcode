#!/usr/bin/env python3
import sys

from collections import defaultdict

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

wires = open(filename).read().splitlines()
for i in range(len(wires)):
    wires[i] = [(j[0], int(j[1:])) for j in wires[i].split(",")]

# way dumber solution but would've probably been much faster to write if I'd
# thought to do this first

grid = defaultdict(lambda: [None, None])
for i, w in enumerate(wires):
    x, y = 0, 0
    steps = 0
    for dir, dist in w:
        for j in range(dist):
            if dir == "R":
                x += 1
            elif dir == "L":
                x -= 1
            elif dir == "D":
                y += 1
            elif dir == "U":
                y -= 1
            else:
                raise Exception("Unknown dir {}".format(dir))
            steps += 1
            grid[x, y][i] = steps if grid[x, y][i] is None else min(steps, grid[x, y][i])

intersections = [(i, j) for i, j in grid.items() if all(x is not None for x in j)]

# star1
print(min(abs(x) + abs(y) for (x, y), _ in intersections))

# star2
print(min(sum(steps) for _, steps in intersections))
