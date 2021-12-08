#!/usr/bin/env python3
import sys
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

DIGITS = [
    "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg"
]


def map(input, mapping):
    return "".join(sorted(mapping[ord(i) - 97] for i in input))


def deduce(input):
    for mapping in itertools.permutations("abcdefg"):
        for i in input:
            j = map(i, mapping)
            if j not in DIGITS:
                break
        else:
            return mapping


lines = [tuple(j.split(" ") for j in i.split(" | ")) for i in open(filename).read().splitlines()]

# star 1
count = 0
for _, digits in lines:
    for digit in digits:
        if len(digit) in (2, 3, 4, 7):
            count += 1
print(count)

# star 2
total = 0
for input, digits in lines:
    mapping = deduce(input)
    total += int("".join(str(DIGITS.index(map(i, mapping))) for i in digits))
print(total)
