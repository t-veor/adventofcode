#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().splitlines()

# star 1
print(sum(int(i) // 3 - 2 for i in data))

# star 2
total = 0
for i in data:
    mass = int(i)
    tot = 0
    next_req = mass

    while True:
        next_req = next_req // 3 - 2
        if next_req <= 0:
            break
        tot += next_req

    total += tot

print(total)

