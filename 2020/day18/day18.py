#!/usr/bin/env python3
import sys
import re

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


exprs = open(filename).read().splitlines()


def lex(string):
    token_regex = re.compile(r"(?:(\d+)|([\(\)+\*]))\s*")

    while string:
        match = token_regex.match(string)
        if not match:
            break
        string = string[len(match.group(0)):]
        if match.group(1):
            yield int(match.group(1))
        else:
            yield match.group(2)


def shunting_yard(tokens, precedence):
    op_stack = []

    for tok in tokens:
        if type(tok) is int:
            yield tok
        elif tok in precedence:
            while op_stack:
                if op_stack[-1] == "(":
                    break
                if precedence[op_stack[-1]] < precedence[tok]:
                    break
                yield op_stack.pop()
            op_stack.append(tok)
        elif tok == "(":
            op_stack.append(tok)
        else:
            while op_stack and op_stack[-1] != "(":
                yield op_stack.pop()
            if op_stack and op_stack[-1] == "(":
                op_stack.pop()

    while op_stack:
        yield op_stack.pop()


def evaluate(string, precedence={ "+": 1, "*": 1 }):
    stack = []

    for x in shunting_yard(lex(string), precedence):
        if type(x) is int:
            stack.append(x)
        elif x == "+":
            x, y = stack.pop(), stack.pop()
            stack.append(x + y)
        else:
            x, y = stack.pop(), stack.pop()
            stack.append(x * y)

    return stack[0]


# star 1
print(sum(evaluate(i) for i in exprs))

# star 2
precedence = { "+": 2, "*": 1 }
print(sum(evaluate(i, precedence) for i in exprs))
