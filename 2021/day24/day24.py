#!/usr/bin/env python3
from functools import reduce
import sys
import re

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

loop_regex = re.compile(r"""inp w
mul x 0
add x z
mod x 26
div z (-?\d+)
add x (-?\d+)
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y (-?\d+)
mul y x
add z y
""")

program = open(filename).read()
constants = []

position = 0
for _ in range(14):
    match = loop_regex.match(program, position)
    if match is None:
        raise Exception("Input is not the format expected!")
    a, b, c = int(match.group(1)), int(match.group(2)), int(match.group(3))
    position = match.end()
    constants.append((a, b, c))

if position != len(program):
    raise Exception("Input is not the format expected!")

constraints = []
stack = []
for i, (a, b, c) in enumerate(constants):
    if a == 1:
        stack.append((i, c))
    else:
        j, acc = stack.pop()
        constraints.append((j, i, acc + b))

constraints.sort()

def maximize(constraints):
    number = [None] * 14
    for i, j, c in constraints:
        a = min(9, 9 - c)
        b = a + c
        number[i] = a
        number[j] = b

    return reduce(lambda acc, x: acc * 10 + x, number)

def minimize(constraints):
    number = [None] * 14
    for i, j, c in constraints:
        a = max(1, 1 - c)
        b = a + c
        number[i] = a
        number[j] = b

    return reduce(lambda acc, x: acc * 10 + x, number)

# star 1
print(maximize(constraints))

# star 2
print(minimize(constraints))
