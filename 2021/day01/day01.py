#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [int(i) for i in open(filename).read().splitlines()]

# star 1
count = 0
for i, j in zip(data, data[1:]):
    if j > i:
        count += 1
print(count)

# star 2
windows = []
for i in range(len(data) - 2):
    windows.append(sum(data[i:i+3]))
count = 0
for i, j in zip(windows, windows[1:]):
    if j > i:
        count += 1
print(count)
