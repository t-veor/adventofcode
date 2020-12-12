#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

instructions = [(i[0], int(i[1:])) for i in open(filename).read().splitlines()]


def exec(instructions):
    x, y = 0, 0
    facing = (1, 0)

    for action, num in instructions:
        if action == "N":
            y += num
        elif action == "S":
            y -= num
        elif action == "E":
            x += num
        elif action == "W":
            x -= num
        elif action == "L":
            for _ in range(0, num, 90):
                facing = (-facing[1], facing[0])
        elif action == "R":
            for _ in range(0, num, 90):
                facing = (facing[1], -facing[0])
        else:
            x += facing[0] * num
            y += facing[1] * num

    return x, y


def exec_waypoint(instructions):
    x, y = 0, 0
    waypoint_x, waypoint_y = 10, 1

    for action, num in instructions:
        if action == "N":
            waypoint_y += num
        elif action == "S":
            waypoint_y -= num
        elif action == "E":
            waypoint_x += num
        elif action == "W":
            waypoint_x -= num
        elif action == "L":
            for _ in range(0, num, 90):
                waypoint_x, waypoint_y = -waypoint_y, waypoint_x
        elif action == "R":
            for _ in range(0, num, 90):
                waypoint_x, waypoint_y = waypoint_y, -waypoint_x
        else:
            x += waypoint_x * num
            y += waypoint_y * num

    return x, y


# star 1
dx, dy = exec(instructions)
print(abs(dx) + abs(dy))


# star 2
dx, dy = exec_waypoint(instructions)
print(abs(dx) + abs(dy))
