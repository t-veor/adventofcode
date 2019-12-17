#!/usr/bin/env python3
# pylint: disable=import-error
import sys
from os.path import dirname, join
sys.path.append(join(dirname(__file__), ".."))

from collections import deque
from intcode import IntCodeVM

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]


def adjacent(pos):
    x, y = pos
    yield 1, (x, y - 1)
    yield 2, (x, y + 1)
    yield 3, (x - 1, y)
    yield 4, (x + 1, y)


OPPOSITE = { 1: 2, 2: 1, 3: 4, 4: 3 }


def bot_dfs(program):
    vm = IntCodeVM(program)
    next_command = 0
    def get_next_command():
        while True:
            yield next_command
    process = vm.run(get_next_command())

    visited = set()
    board = {}
    dists = {}
    oxygen_location = None

    def recurse(pos, n):
        nonlocal next_command, oxygen_location
        visited.add(pos)
        for command, new_pos in adjacent(pos):
            if new_pos not in visited:
                # try moving to new_pos
                next_command = command
                bot_status = next(process)

                board[new_pos] = bot_status
                if bot_status == 0:
                    # hit a wall
                    continue
                elif bot_status == 1:
                    pass
                else:
                    # found the oxygen system
                    oxygen_location = new_pos
                dists[new_pos] = n + 1

                recurse_status = recurse(new_pos, n + 1)
                if recurse_status is not None:
                    return recurse_status

                # move back
                next_command = OPPOSITE[command]
                next(process)

    board[0, 0] = 1
    recurse((0, 0), 0)

    return board, oxygen_location, dists[oxygen_location]


def oxygen_bfs(board, starting_pos):
    visited = set()
    frontier = deque()
    max_dist = 0
    frontier.append((starting_pos, 0))
    visited.add(starting_pos)

    while frontier:
        pos, dist = frontier.popleft()
        max_dist = max(dist, max_dist)
        for _, new_pos in adjacent(pos):
            if board.get(new_pos, 0) != 0 and new_pos not in visited:
                visited.add(new_pos)
                frontier.append((new_pos, dist + 1))

    return max_dist



board, oxygen_location, oxygen_dist = bot_dfs(program)

# star 1
print(oxygen_dist)

# star 2
print(oxygen_bfs(board, oxygen_location))
