#!/usr/bin/env python3
import sys
import heapq

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

grid = [[int(j) for j in i] for i in open(filename).read().strip().splitlines()]
grid_width, grid_height = len(grid[0]), len(grid)

def adjacent(p, grid):
    grid_width, grid_height = len(grid[0]), len(grid)
    x, y = p
    if x > 0:
        yield (x - 1, y)
    if x + 1 < grid_width:
        yield (x + 1, y)
    if y > 0:
        yield (x, y - 1)
    if y + 1 < grid_height:
        yield (x, y + 1)


def heuristic(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def a_star(grid, start, end):
    queue = []
    comes_from = {}
    seen = set()

    queue.append((0, start))

    while queue:
        cost, point = heapq.heappop(queue)
        if point == end:
            break

        if point in seen:
            continue

        seen.add(point)

        for adjacent_point in adjacent(point, grid):
            x, y = adjacent_point
            new_cost = cost + grid[y][x]
            if adjacent_point not in comes_from or new_cost < comes_from[adjacent_point][0]:
                comes_from[adjacent_point] = (new_cost, point)
                heapq.heappush(queue, (new_cost, adjacent_point))

    # curr_point = end
    # reversed_path = [end]
    # while curr_point != start:
    #     curr_point = comes_from[curr_point][1]
    #     reversed_path.append(curr_point)

    return comes_from[end][0]


def hstack(arrs):
    return [[k for j in i for k in j] for i in zip(*arrs)]


def incr_grid(grid, x):
    return [[(i + x - 1) % 9 + 1 for i in row] for row in grid]


def expand_grid(grid):
    new_grid = []
    for y in range(5):
        stack = []
        for x in range(5):
            stack.append(incr_grid(grid, x + y))
        new_grid += hstack(stack)
    return new_grid


# star 1
print(a_star(grid, (0, 0), (grid_width - 1, grid_height - 1)))

# star 2
expanded_grid = expand_grid(grid)
print(a_star(expanded_grid, (0, 0), (5 * grid_width - 1, 5 * grid_height - 1)))
