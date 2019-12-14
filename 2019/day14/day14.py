#!/usr/bin/env python3
import sys
import math
from collections import defaultdict

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

reactions = {}
for i in open(filename).read().splitlines():
    r, p = i.split(" => ")
    reagents = {}
    for j in r.split(", "):
        k, l = j.split(" ")
        reagents[l] = int(k)
    quantity, product = p.split(" ")
    reactions[product] = (reagents, int(quantity))


def topo_sort(reactions):
    result = []
    visited = set()

    def visit(node):
        if node in visited:
            return
        parents, _ = reactions.get(node, ({}, 0))
        for i in parents:
            visit(i)
        visited.add(node)
        result.append(node)

    for i in reactions:
        visit(i)

    return result


def ore_req(reactions, required=1):
    requirements = defaultdict(lambda: 0)
    order = list(reversed(topo_sort(reactions)))
    requirements[order[0]] = required

    for i in order:
        if i in reactions:
            req = requirements[i]
            reagents, qty = reactions[i]
            num_reactions = math.ceil(req / qty)
            for i in reagents:
                requirements[i] += reagents[i] * num_reactions

    return requirements[order[-1]]


def exponential_search(f, target):
    bound = 1
    # expand
    while f(bound) <= target:
        bound *= 2

    # regular binary search
    min_bound, max_bound = bound // 2, bound
    while min_bound < max_bound:
        median = (min_bound + max_bound) // 2
        result = f(median)
        if result <= target:
            min_bound = median + 1
        else:
            max_bound = median
    # min bound is the first x for which f(x) > target
    return min_bound - 1


# star 1
print(ore_req(reactions))

# star 2
one_trillion = 1000000000000
print(exponential_search(lambda i: ore_req(reactions, i), one_trillion))
