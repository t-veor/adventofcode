#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [int(i) for i in open(filename).read().splitlines()]

# star 1
invalid_number = 0
for i, a in enumerate(data):
    if i < 25:
        continue
    is_sum = False
    for j in range(25):
        for k in range(j + 1, 25):
            b, c = data[i - 25 + j], data[i - 25 + k]
            if b + c == a:
                is_sum = True
                break
        if is_sum:
            break
    if not is_sum:
        invalid_number = a
        print(a)
        break


found = False
for start in range(len(data)):
    current_sum = 0
    for end in range(start, len(data) + 1):
        if end != start:
            current_sum += data[end - 1]
        if current_sum == invalid_number:
            contiguous_range = data[start:end]
            minimum = min(contiguous_range)
            maximum = max(contiguous_range)
            print(minimum + maximum)
            found = True
            break
    if found:
        break
