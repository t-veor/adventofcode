#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

positions = [int(i) for i in open(filename).read().strip().split(",")]
positions.sort()

# star 1
# just pick (a) median - the median means half the crabs are on one side, half
# the crabs are on the other, so local perturbations to this alignment point
# don't change the fuel requirement. This is a local minimum and it's pretty
# easy to see this has to be the global minimum too
median = positions[len(positions) // 2]
print(sum(abs(median - i) for i in positions))

# star 2
# I couldn't think of a cool mathematical trick for this one
def triangle(n):
    return n * (n + 1) // 2
fuel_reqs = []
for i in range(positions[0], positions[-1] + 1):
    fuel = 0
    for j in positions:
        fuel += triangle(abs(j - i))
    fuel_reqs.append(fuel)
print(min(fuel_reqs))
