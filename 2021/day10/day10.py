#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

lines = open(filename).read().strip().splitlines()

PAIRS = {
    "(": ")",
    "[": "]",
    "{": "}",
    "<": ">",
}

SYNTAX_SCORES = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}

COMPLETE_SCORES = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}

def score_line(line):
    stack = []
    for c in line:
        if stack and c == stack[-1]:
            stack.pop()
        elif c in PAIRS:
            stack.append(PAIRS[c])
        else:
            return (SYNTAX_SCORES[c], None)
    return (0, reversed(stack))


def score_completion(completion):
    score = 0
    for c in completion:
        score = 5 * score + COMPLETE_SCORES[c]
    return score

# star 1
print(sum(score_line(l)[0] for l in lines))

# star 2
scores = []
for l in lines:
    s, c = score_line(l)
    if s == 0:
        scores.append(score_completion(c))
scores.sort()
print(scores[len(scores) // 2])
