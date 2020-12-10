#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

jolts = [int(i) for i in open(filename).read().splitlines()]

# star 1
device_jolts = max(jolts) + 3
sorted_jolts = sorted(jolts + [0, device_jolts])
one_count = 0
three_count = 0
for i, j in zip(sorted_jolts, sorted_jolts[1:]):
    if j - i == 1:
        one_count += 1
    elif j - i == 3:
        three_count += 1
print(one_count * three_count)

# star 2
arrangement_count = [0] * len(sorted_jolts)
arrangement_count[0] = 1
for i in range(1, len(sorted_jolts)):
    current_jolt = sorted_jolts[i]
    for j in range(i - 1, -1, -1):
        if current_jolt - sorted_jolts[j] > 3:
            break
        arrangement_count[i] += arrangement_count[j]
print(arrangement_count[-1])
