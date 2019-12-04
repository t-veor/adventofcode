#!/usr/bin/env python3
import sys
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

begin, end = [int(i) for i in open(filename).read().split("-")]

# star 1
count = 0
for i in range(begin, end + 1):
    password = str(i)
    has_double = False
    for i, j in zip(password, password[1:]):
        if i > j:
            break
        if i == j:
            has_double = True
    else:
        if has_double:
            count += 1
print(count)

# star 2
count = 0
for i in range(begin, end + 1):
    password = str(i)
    for i, j in zip(password, password[1:]):
        if i > j:
            break
    else:
        groups = [list(i) for _, i in itertools.groupby(password)]
        if any(len(i) == 2 for i in groups):
            count += 1
print(count)
