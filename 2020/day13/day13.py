#!/usr/bin/env python3
import sys
import math

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

earliest, buses = open(filename).read().splitlines()
earliest = int(earliest)
buses = [int(i) if i.isdigit() else None for i in buses.split(",")]


def egcd(b, n):
    # taken from https://github.com/lapets/egcd/blob/master/egcd/egcd.py
    (x0, x1, y0, y1) = (1, 0, 0, 1)
    while n != 0:
        (q, b, n) = (b // n, n, b % n)
        (x0, x1) = (x1, x0 - q * x1)
        (y0, y1) = (y1, y0 - q * y1)
    return (b, x0, y0)


# star 1
earliest_id = None
earliest_time = None
for i in buses:
    if i is None:
        continue
    curr_earliest_time = math.ceil(earliest / i) * i
    if earliest_time is None or curr_earliest_time < earliest_time:
        earliest_id = i
        earliest_time = curr_earliest_time
print(earliest_id * (earliest_time - earliest))

# star 2
# oh god, it's chinese remainder theorem
equations = []
for i, j in enumerate(buses):
    if j is not None:
        equations.append((-i % j, j))

a, n = 0, 1
for a_1, n_1 in equations:
    r, m_0, m_1 = egcd(n, n_1)
    assert(r == 1)
    a = a * m_1 * n_1 + a_1 * m_0 * n
    n *= n_1
    a %= n
print(a)
