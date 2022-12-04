#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [
    tuple(tuple(int(j) for j in i.split("-")) for i in line.split(","))
    for line in open(filename).read().splitlines()
]


def intersect(a, b):
    return (max(a[0], b[0]), min(a[1], b[1]))

# star 1
num_fully_contained = 0
for a, b in data:
    intersection = intersect(a, b)
    if a == intersection or b == intersection:
        num_fully_contained += 1
print(num_fully_contained)

# star 2
num_overlaps = 0
for a, b in data:
    intersection = intersect(a, b)
    if intersection[1] >= intersection[0]:
        num_overlaps += 1
print(num_overlaps)
