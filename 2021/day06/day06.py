#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

timers = [int(i) for i in open(filename).read().strip().split(",")]

fish = [0] * 7
for i in timers:
    fish[i] += 1

a, b = 0, 0
for i in range(256):
    index = i % 7
    c = fish[index]
    fish[index] += a
    a, b = b, c

    # star 1
    if i == 79:
        print(sum(fish) + a + b)

# star 2
print(sum(fish) + a + b)
