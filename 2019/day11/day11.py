#!/usr/bin/env python3
# pylint: disable=import-error
import sys
from os.path import dirname, join
sys.path.append(join(dirname(__file__), ".."))

from collections import defaultdict
from intcode import IntCodeVM

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]


def run_robot(program, start=0):
    board = defaultdict(lambda: 0)
    painted = set()
    x, y = 0, 0
    board[x, y] = start
    dx, dy = 0, -1
    vm = IntCodeVM(program)

    def camera_out():
        while True:
            yield board[x, y]

    process = vm.run(camera_out())

    try:
        while True:
            board[x, y] = next(process)
            painted.add((x, y))
            turn = next(process)
            if turn == 0:
                dx, dy = dy, -dx
            else:
                dx, dy = -dy, dx
            x += dx
            y += dy
    except StopIteration:
        pass

    return board, painted


def print_board(board):
    max_x = max(i[0] for i in board.keys())
    min_x = min(i[0] for i in board.keys())
    max_y = max(i[1] for i in board.keys())
    min_y = min(i[1] for i in board.keys())

    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            print("\u2588" if board[x, y] else " ", end="")
        print()

# star 1
print(len(run_robot(program)[1]))

# star 2
print_board(run_robot(program, start=1)[0])
