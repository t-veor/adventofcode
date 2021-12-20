#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

with open(filename) as f:
    rule = [c == "#" for c in f.readline().strip()]
    f.readline()
    initial_grid = [[c == "#" for c in line] for line in f.read().strip().splitlines()]

class CellGrid:
    def __init__(self):
        self.grid = {}
        self.untouched_cell_state = False
        self.min_x = 0
        self.max_x = 0
        self.min_y = 0
        self.max_y = 0

    @staticmethod
    def from_grid(grid):
        result = CellGrid()
        for y, row in enumerate(grid):
            for x, cell in enumerate(row):
                result[x, y] = cell
        return result

    def __getitem__(self, key):
        return self.grid.get(key, self.untouched_cell_state)

    def __setitem__(self, key, value):
        x, y = key
        self.grid[key] = value
        self.min_x = min(x, self.min_x)
        self.max_x = max(x, self.max_x)
        self.min_y = min(y, self.min_y)
        self.max_y = max(y, self.max_y)

    def index_at(self, key):
        x, y = key
        index = 0
        for y_ in range(y - 1, y + 2):
            for x_ in range(x - 1, x + 2):
                index *= 2
                if self[x_, y_]:
                    index += 1
        return index

    def step(self, rule):
        new_state = CellGrid()
        for y in range(self.min_y - 1, self.max_y + 2):
            for x in range(self.min_x - 1, self.max_x + 2):
                index = self.index_at((x, y))
                new_state[x, y] = rule[index]

        if not self.untouched_cell_state:
            new_state.untouched_cell_state = rule[0]
        else:
            new_state.untouched_cell_state = rule[-1]

        return new_state

    def lit_cells(self):
        if self.untouched_cell_state:
            raise "Infinite number of lit cells!"

        count = 0
        for cell in self.grid.values():
            if cell:
                count += 1
        return count

    def print(self):
        for y in range(self.min_y - 1, self.max_y + 2):
            for x in range(self.min_x - 1, self.max_x + 2):
                print("#" if self[x, y] else ".", end="")
            print()



initial_state = CellGrid.from_grid(initial_grid)

# star 1
state = initial_state
for _ in range(2):
    state = state.step(rule)
print(state.lit_cells())

# star 2
state = initial_state
for _ in range(50):
    state = state.step(rule)
print(state.lit_cells())
