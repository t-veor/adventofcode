#!/usr/bin/env python3
import sys
import re

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


def parse_rule(rule: str):
    parent, rest = rule.split(" contain ")
    parent_name = parent[:-5]

    children = rest.split(", ")
    child_nodes = []
    for i in children:
        if i.startswith("no"):
            break

        words = i.split(" ")
        weight = int(words[0])
        name = " ".join(words[1:-1])
        child_nodes.append((weight, name))

    return (parent_name, child_nodes)


rules = [parse_rule(i) for i in open(filename).read().splitlines()]

forward_tree = {}
backward_tree = {}

for parent, children in rules:
    forward_tree[parent] = children

    if parent not in backward_tree:
        backward_tree[parent] = []

    for weight, child in children:
        if child not in backward_tree:
            backward_tree[child] = []
        backward_tree[child].append((weight, parent))


def get_all_children(tree, node):
    children = set()
    def walk(node):
        for _, child in tree[node]:
            if child not in children:
                children.add(child)
                walk(child)
    walk(node)

    return children


def bag_count(tree, node):
    def walk(node):
        total = 1
        for count, child in tree[node]:
            total += count * walk(child)
        return total

    return walk(node)


# star 1
print(len(get_all_children(backward_tree, "shiny gold")))

# star 2
# the -1 is because bag_count counts the outer shiny gold bag
print(bag_count(forward_tree, "shiny gold") - 1)
