#!/usr/bin/env python3
import sys
import itertools
from collections import defaultdict
import math

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

asteroids = []
for y, l in enumerate(open(filename).read().splitlines()):
    for x, i in enumerate(l):
        if i == "#":
            asteroids.append((x, y))


def canonicalize_angle(frac):
    num, denom = frac
    divisor = math.gcd(num, denom)
    if divisor == 0:
        divisor = max(abs(num), abs(denom))
    return (num // divisor, denom // divisor)


def num_visible(position):
    unique_visible = set()
    x1, y1 = position
    for (x2, y2) in asteroids:
        dx, dy = x2 - x1, y2 - y1
        if (dx, dy) != (0, 0):
            unique_visible.add(canonicalize_angle((dx, dy)))
    return len(unique_visible)


def radians_from_y_axis(angle):
    x, y = angle
    theta = -math.atan2(-y, x) + math.pi / 2
    theta %= math.pi * 2
    return theta


def destroy_all_asteroids(position):
    x1, y1 = position
    to_destroy = {}
    for (x2, y2) in asteroids:
        dx, dy = x2 - x1, y2 - y1
        if (dx, dy) == (0, 0):
            continue
        canonical_angle = canonicalize_angle((dx, dy))
        if canonical_angle not in to_destroy:
            to_destroy[canonical_angle] = []
        to_destroy[canonical_angle].append((x2, y2))

    for i in to_destroy:
        to_destroy[i].sort(key=lambda j: (j[0] - x1) ** 2 + (j[1] - y1) ** 2)

    destroy_buckets = list(to_destroy.items())
    destroy_buckets.sort(key=lambda i: radians_from_y_axis(i[0]))

    while destroy_buckets:
        for i, j in destroy_buckets:
            if j:
                yield j.pop(0)
        destroy_buckets = [(i, j) for i, j in destroy_buckets if j]

# star 1
station_coords = max(asteroids, key=num_visible)
print(num_visible(station_coords))

# star 2
destroy_order = list(destroy_all_asteroids(station_coords))
print(destroy_order[199][0] * 100 + destroy_order[199][1])
