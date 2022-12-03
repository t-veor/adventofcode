#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [i for i in open(filename).read().splitlines()]

def priority(item):
    item = ord(item)
    if ord("A") <= item <= ord("Z"):
        return item - ord("A") + 27
    else:
        return item - ord("a") + 1

# star 1
total_priority = 0
for line in data:
    half = len(line) // 2
    first, second = line[:half], line[half:]
    common = set(first) & set(second)
    total_priority += sum(priority(i) for i in common)
print(total_priority)

# star 2
total_priority = 0
for i in range(0, len(data), 3):
    common = set(data[i]) & set(data[i + 1]) & set(data[i + 2])
    total_priority += sum(priority(i) for i in common)
print(total_priority)
