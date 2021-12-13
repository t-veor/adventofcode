#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().strip()
dots, folds = data.split("\n\n")
dots = set(tuple(int(j) for j in i.split(",")) for i in dots.splitlines())
folds = [(i[11], int(i[13:])) for i in folds.splitlines()]

def fold(dots, line):
    reflect = lambda n: min(n, 2 * line[1] - n)
    if line[0] == "x":
        transform = lambda p: (reflect(p[0]), p[1])
    else:
        transform = lambda p: (p[0], reflect(p[1]))

    new_dots = set()
    for p in dots:
        new_dots.add(transform(p))

    return new_dots


def print_dots(dots):
    min_x = min(x for x, _ in dots)
    max_x = max(x for x, _ in dots)
    min_y = min(y for _, y in dots)
    max_y = max(y for _, y in dots)

    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            if (x, y) in dots:
                print("#", end="")
            else:
                print(" ", end="")
        print()

# star 1
print(len(fold(dots, folds[0])))

# star 2
folded_dots = dots
for line in folds:
    folded_dots = fold(folded_dots, line)
print_dots(folded_dots)
