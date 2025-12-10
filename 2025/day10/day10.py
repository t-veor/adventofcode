# /// script
# dependencies = [
#     "z3-solver"
# ]
# ///

from functools import reduce
from operator import add, xor

from z3 import *


def parse_problems(input):
    problems = []

    for line in input.splitlines():
        a, *b, c = line.split(" ")

        a = [i == "#" for i in a[1:-1]]
        b = [[int(j) for j in i[1:-1].split(",")] for i in b]
        c = [int(i) for i in c[1:-1].split(",")]

        problems.append((a, b, c))

    return problems


def solve_configure(problem):
    totals, connections, _ = problem

    s = Optimize()

    vars = Bools(" ".join(f"x{i}" for i in range(len(connections))))

    for i, total in enumerate(totals):
        this_vars = []
        for j, conn_j in enumerate(connections):
            if i in conn_j:
                this_vars.append(vars[j])

        if total != 0 and not this_vars:
            raise Exception(f"???? wtf {connections} {totals}")

        s.add(reduce(xor, this_vars) == total)

    s.minimize(reduce(add, vars))

    assert s.check() == sat, f"??? wtf not solvable? {connections} {totals}"
    model = s.model()

    return sum(bool(model[i]) for i in vars)


def solve_joltage(problem):
    _, connections, totals = problem

    s = Optimize()

    vars = Ints(" ".join(f"x{i}" for i in range(len(connections))))

    for i, total in enumerate(totals):
        this_vars = []
        for j, conn_j in enumerate(connections):
            if i in conn_j:
                this_vars.append(vars[j])

        if total != 0 and not this_vars:
            raise Exception(f"???? wtf {connections} {totals}")

        s.add(reduce(add, this_vars) == total)

    for var in vars:
        s.add(var >= 0)

    s.minimize(reduce(add, vars))

    assert s.check() == sat, f"??? wtf not solvable? {connections} {totals}"
    model = s.model()

    return sum(model[i].as_long() for i in vars)


def star1(problems):
    return sum(solve_configure(i) for i in problems)


def star2(problems):
    return sum(solve_joltage(i) for i in problems)


with open("input.txt") as f:
    problems = parse_problems(f.read())

print(star1(problems))
print(star2(problems))
