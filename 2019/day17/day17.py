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


def adjacent(pos):
    x, y = pos
    yield x + 1, y
    yield x - 1, y
    yield x, y + 1
    yield x, y - 1


def get_view(program):
    vm = IntCodeVM(program)

    scaffolding = set()
    robot = None
    x, y = 0, 0
    for i in vm.run(iter(())):
        c = chr(i)
        if c == "\n":
            y += 1
            x = 0
            continue
        elif c == "#":
            scaffolding.add((x, y))
        elif c in "^>v<":
            robot = (x, y), c

        x += 1

    return scaffolding, robot


def print_view(scaffolding, robot):
    max_x = max(i[0] for i in scaffolding)
    max_y = max(i[1] for i in scaffolding)
    intersections = set(scaffolding_intersections(scaffolding))

    for y in range(max_y + 1):
        for x in range(max_x + 1):
            if (x, y) == robot[0]:
                print(robot[1], end="")
            elif (x, y) in intersections:
                print("O", end="")
            elif (x, y) in scaffolding:
                print("#", end="")
            else:
                print(".", end="")
        print()


def scaffolding_intersections(scaffolding):
    for pos in scaffolding:
        for adj in adjacent(pos):
            if adj not in scaffolding:
                break
        else:
            yield pos


def raw_movement_instructions(scaffolding, robot):
    # by visual inspection of my input, it looks like I don't have to do
    # eulerian path detection, as part 1 seemed to suggest I might
    delta = {"^": (0, -1), ">": (1, 0), "v": (0, 1), "<": (-1, 0)}

    x, y = robot[0]
    dx, dy = delta[robot[1]]

    # just walk as far as you can until you can't any more
    forward = 0
    while True:
        if (x + dx, y + dy) in scaffolding:
            forward += 1
            x += dx
            y += dy
        elif (x + dy, y - dx) in scaffolding:
            # turn left
            if forward > 0:
                yield str(forward)
            forward = 0
            yield "L"
            dx, dy = dy, -dx
        elif (x - dy, y + dx) in scaffolding:
            # turn right
            if forward > 0:
                yield str(forward)
            forward = 0
            yield "R"
            dx, dy = -dy, dx
        else:
            # end of the line
            if forward > 0:
                yield str(forward)
            return



# star 1
scaffolding, robot = get_view(program)
# print_view(scaffolding, robot)
alignment = 0
for x, y in scaffolding_intersections(scaffolding):
    alignment += x * y
print(alignment)

# star 2
# this was my initial solution - it was faster to do it by hand than figure out
# an algorithm to do it
# print(",".join(raw_movement_instructions(scaffolding, robot)))
program[0] = 2
vm = IntCodeVM(program)
i = "B,A,A,B,C,B,C,B,C,A\nR,8,R,12,L,12\nL,6,R,12,R,8\nR,12,L,12,L,4,L,4\nn\n"
print(list(vm.run(ord(j) for j in i))[-1])
