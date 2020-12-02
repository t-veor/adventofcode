#!/usr/bin/env python3
import sys
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [int(i) for i in open(filename).read().splitlines()]

# star 1
for (i, j) in itertools.combinations(data, 2):
    if i + j == 2020:
        print(i * j)
        break

# star 2
for (i, j, k) in itertools.combinations(data, 3):
    if i + j + k == 2020:
        print(i * j * k)
        break
