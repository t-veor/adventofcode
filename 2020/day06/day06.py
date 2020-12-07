#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


def group_by(iter, sep):
    current_group = []
    for i in iter:
        if i == sep:
            yield current_group
            current_group = []
        else:
            current_group.append(i)
    yield current_group


groups = [i for i in group_by(open(filename).read().splitlines(), "") if i]

# star 1
total = 0
for group in groups:
    questions = set()
    for i in group:
        questions |= set(i)
    total += len(questions)
print(total)

# star 2
total = 0
for group in groups:
    questions = set(group[0])
    for i in group[1:]:
        questions &= set(i)
    total += len(questions)
print(total)
