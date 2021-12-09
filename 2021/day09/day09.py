#!/usr/bin/env python3
from collections import deque
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

grid = [[int(j) for j in i] for i in open(filename).read().strip().splitlines()]
grid_width, grid_height = len(grid[0]), len(grid)


def adjacent(x, y):
    if x > 0:
        yield (x - 1, y)
    if x + 1 < grid_width:
        yield (x + 1, y)
    if y > 0:
        yield (x, y - 1)
    if y + 1 < grid_height:
        yield (x, y + 1)


def flood_fill_basin(min_x, min_y):
    queue = deque()
    queue.append((min_x, min_y))
    basin_points = set()

    while queue:
        x, y = queue.popleft()
        # because we're guaranteed every point is in exactly one basin there's
        # no need to actually check the height value unless it's 9
        if grid[y][x] == 9:
            continue

        if (x, y) in basin_points:
            continue

        basin_points.add((x, y))
        for i in adjacent(x, y):
            queue.append(i)

    return len(basin_points)


# star 1
minima = []
for y in range(grid_height):
    for x in range(grid_width):
        height = grid[y][x]
        for (x2, y2) in adjacent(x, y):
            if grid[y2][x2] <= height:
                break
        else:
            minima.append((x, y))
print(sum(grid[y][x] + 1 for x, y in minima))

# star 2
basins = [flood_fill_basin(*i) for i in minima]
basins.sort()
a, b, c = basins[-3:]
print(a * b * c)
