#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


def sequence_generator(initial):
    last_seen = {}
    last_num = None
    index = 0

    while True:
        if index < len(initial):
            num = initial[index]
        elif last_num not in last_seen:
            num = 0
        else:
            num = index - last_seen[last_num]
        yield num
        last_seen[last_num] = index
        last_num = num
        index += 1


input_sequence = [int(i) for i in open(filename).read().splitlines()[0].split(",")]

for i, j in enumerate(sequence_generator(input_sequence)):
    # star 1
    if i == 2019:
        print(j)

    # star 2
    if i == 29999999:
        print(j)
        break
