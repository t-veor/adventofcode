#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

boarding_passes = open(filename).read().splitlines()


def seat_id(boarding_pass):
    id = boarding_pass \
        .replace("F", "0") \
        .replace("B", "1") \
        .replace("L", "0") \
        .replace("R", "1")
    return int(id, 2)


seat_ids = [seat_id(i) for i in boarding_passes]

# star 1
print(max(seat_ids))

# star 2
seat_ids.sort()
for i in range(len(seat_ids) - 1):
    if seat_ids[i + 1] - seat_ids[i] != 1:
        print(seat_ids[i] + 1)
        break
