#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

instructions = [(i, int(j)) for i, j in (i.split() for i in open(filename).read().splitlines())]

# star 1
x = 0
y = 0
for instr, mag in instructions:
    if instr == "forward":
        x += mag
    elif instr == "up":
        y -= mag
    else:
        y += mag
print(x * y)

# star 2
x = 0
y = 0
aim = 0
for instr, mag in instructions:
    if instr == "forward":
        x += mag
        y += mag * aim
    elif instr == "up":
        aim -= mag
    else:
        aim += mag
print(x * y)
