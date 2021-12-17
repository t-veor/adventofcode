#!/usr/bin/env python3
import sys
from math import sqrt, ceil

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().strip()
data = data[len("target area: x="):]
ranges = data.split(", y=")
x_range, y_range = [tuple(int(j) for j in i.split("..")) for i in ranges]

# assumptions - the x_range is all positive and the y_range is all negative
assert x_range[0] < x_range[1], "y range is inverted!"
assert x_range[0] > 0, "x range must be entirely positive!"
assert y_range[0] < y_range[1], "y range is inverted!"
assert y_range[1] < 0, "y range must be entirely negative!"

def triangle(n):
    return (n * (n + 1)) // 2

def sgn(x):
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0


def velocity_in_range(vel, x_range, y_range):
    min_x, max_x = x_range
    min_y, max_y = y_range
    x_vel, y_vel = vel
    x, y = 0, 0

    while True:
        x += x_vel
        y += y_vel
        x_vel -= sgn(x_vel)
        y_vel -= 1
        if min_x <= x <= max_x and min_y <= y <= max_y:
            return True

        if x > max_x:
            return False

        if y < min_y and y_vel < 0:
            return False


# star 1
# the y trajectory always passes through 0 on the way down, and in the next step
# steps (-initial_y_vel - 1), so the maximum positive velocity that works is
# just abs(min_y) - 1
max_y_velocity = abs(y_range[0]) - 1
max_height = triangle(max_y_velocity)
print(max_height)

# star 2
# we know that x velocities are bounded above by x_range[1] as otherwise we'd
# travel over the target area in one step, and bounded below by 0 otherwise we
# won't travel in the direction of the target area at all.
# likewise, we know the the minimum y velocity must be just be equal to
# y_range[0] as otherwise we'll move past the target area in one step, and we
# have the max y velocity already, so we can just do a scan:
possible_velocities = 0
for x_vel in range(0, x_range[1] + 1):
    for y_vel in range(y_range[0], max_y_velocity + 1):
        if velocity_in_range((x_vel, y_vel), x_range, y_range):
            possible_velocities += 1
print(possible_velocities)
