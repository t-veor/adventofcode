#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

paths = [tuple(i.split("-")) for i in open(filename).read().strip().splitlines()]

graph = {}
for x, y in paths:
    if x not in graph:
        graph[x] = []
    graph[x].append(y)
    if y not in graph:
        graph[y] = []
    graph[y].append(x)


def dfs(graph):
    found_paths = []

    def recurse(current_node, current_path):
        for node in graph[current_node]:
            if node == "end":
                found_paths.append(current_path + [current_node])
                continue

            if node.islower() and node in current_path:
                continue

            recurse(node, current_path + [current_node])

    recurse("start", [])

    return found_paths


def dfs2(graph):
    found_paths = []

    def recurse(current_node, current_path, small_cave_used):
        for node in graph[current_node]:
            if node == "end":
                found_paths.append(current_path + [current_node])
                continue

            if node == "start":
                continue

            will_use_small_cave = small_cave_used
            if node.islower():
                if node in current_path:
                    if small_cave_used:
                        continue
                    will_use_small_cave = True

            recurse(node, current_path + [current_node], will_use_small_cave)

    recurse("start", [], False)

    return found_paths

# star 1
print(len(dfs(graph)))

# star 1
print(len(dfs2(graph)))
