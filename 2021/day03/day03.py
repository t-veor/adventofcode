#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().splitlines()

# star 1
counts = [[0, 0] for _ in range(len(data[0]))]
for i in data:
    for j, k in enumerate(i):
        counts[j][int(k)] += 1

gamma = int("".join("0" if i > j else "1" for i, j in counts), base=2)
epsilon = int("".join("1" if i > j else "0" for i, j in counts), base=2)
print(gamma * epsilon)

# star 2
def filter_by_bit(data, bit, negate):
    ones = 0
    for i in data:
        ones += int(i[bit])
    zeroes = len(data) - ones

    criteria = "1" if negate ^ (ones >= zeroes) else "0"
    return list(filter(lambda i: i[bit] == criteria, data))


def filter_until_one(data, negate):
    bit = 0
    while True:
        data = filter_by_bit(data, bit, negate)
        if len(data) <= 1:
            break
        bit += 1

    return data[0]


oxygen = int(filter_until_one(data, False), base=2)
co2 = int(filter_until_one(data, True), base=2)
print(oxygen * co2)
