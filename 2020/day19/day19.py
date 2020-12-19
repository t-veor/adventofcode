#!/usr/bin/env python3
import sys
import re

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

input = open(filename).read().splitlines()
split_index = input.index("")
rules_raw, messages = input[:split_index], input[split_index+1:]

rules = {}
for i, j in enumerate(rules_raw):
    label, body = j.split(": ")
    body = [k.split(" ") for k in body.split(" | ")]
    rules[label] = body


def recursive_descent_parser(rules, root, string):
    def descend(label, index):
        body = rules[label]
        for subrule in body:
            # left recursive check
            if subrule[0] == label:
                raise "grammar is left recursive!"

            yield from match_subrule(subrule, index)

    def match_subrule(subrule, index):
        if len(subrule) == 0:
            yield index
        else:
            token, rest = subrule[0], subrule[1:]
            if token.startswith('"'):
                # terminal
                if index < len(string) and string[index] == token[1]:
                    yield from match_subrule(rest, index + 1)
            else:
                for new_index in descend(token, index):
                    yield from match_subrule(rest, new_index)

    for parsed in descend(root, 0):
        if parsed == len(string):
            return True
    return False


# star 1
count = 0
for message in messages:
    if recursive_descent_parser(rules, "0", message):
        count += 1
print(count)


# star 2
rules["8"] = [["42"], ["42", "8"]]
rules["11"] = [["42", "31"], ["42", "11", "31"]]
count = 0
for message in messages:
    if recursive_descent_parser(rules, "0", message):
        count += 1
print(count)
