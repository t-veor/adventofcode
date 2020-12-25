#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

g = 7
n = 20201227

ga, gb = [int(i) for i in open(filename).read().splitlines()]

# star 1
# just work out what a is by brute force
a = -1
x = 1
for i in range(n):
    if x == ga:
        a = i
        break
    x = (x * g) % n

# I'm too lazy to implement the log(n) time exponentiation right now
gab = 1
for _ in range(a):
    gab = (gab * gb) % n
print(gab)

