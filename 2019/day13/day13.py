#!/usr/bin/env python3
# pylint: disable=import-error
import sys
from os.path import dirname, join
sys.path.append(join(dirname(__file__), ".."))

from intcode import IntCodeVM

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]


def sgn(x):
    if x == 0:
        return 0
    elif x < 0:
        return -1
    else:
        return 1


def run_game(program):
    program[0] = 2
    # screen = {}
    score = 0
    vm = IntCodeVM(program)
    max_x = 0
    max_y = 0
    ball_x = 0
    paddle_x = 0

    def get_input():
        while True:
            """
            for y in range(max_y + 1):
                for x in range(max_x + 1):
                    tile = screen.get((x, y), 0)
                    if tile == 0:
                        p = " "
                    elif tile == 1:
                        p = "\u2588"
                    elif tile == 2:
                        p = "\u2592"
                    elif tile == 3:
                        p = "-"
                    else:
                        p = "o"
                    print(p, end="")
                print()
            print("Score: {}".format(score))
            """
            yield sgn(ball_x - paddle_x)

    process = vm.run(get_input())

    try:
        while True:
            x, y, tile = next(process), next(process), next(process)
            if x == -1 and y == 0:
                score = tile
            else:
                max_x = max(x, max_x)
                max_y = max(y, max_y)
                # screen[x, y] = tile

                if tile == 3:
                    paddle_x = x
                elif tile == 4:
                    ball_x = x
    except StopIteration:
        pass

    return score


# star 1
vm = IntCodeVM(program)
blocks = set()
try:
    process = vm.run(iter(()))
    while True:
        x, y, id = next(process), next(process), next(process)
        if id == 2:
            blocks.add((x, y))
except StopIteration:
    pass
print(len(blocks))

# star 2
print(run_game(program))
