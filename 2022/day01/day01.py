#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

elves = []
for line in open(filename):
    line = line.strip()
    if line == "":
        elves.append([])
    else:
        if not elves:
            elves.append([])
        elves[-1].append(int(line))

calories_per_elf = [sum(elf) for elf in elves]

# star 1
print(max(calories_per_elf))

# star 2
print(sum(sorted(calories_per_elf)[-3:]))
