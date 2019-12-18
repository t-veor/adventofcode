#!/usr/bin/env python3
import sys
from collections import defaultdict
from queue import PriorityQueue

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

# This is very very unoptimised!

board = {}
keys = {}
start = None
for y, l in enumerate(open(filename).read().splitlines()):
    for x, c in enumerate(l):
        board[x, y] = c
        if c.islower():
            keys[x, y] = c
        elif c == "@":
            start = x, y


def cardinals(pos):
    x, y = pos
    yield (x + 1, y)
    yield (x - 1, y)
    yield (x, y + 1)
    yield (x, y - 1)


def adjacent(board, pos):
    for i in cardinals(pos):
        if board.get(i, "#") != "#":
            yield i


def all_keys_from(board, start):
    visited = set()
    found_paths = defaultdict(lambda: defaultdict(lambda: float("inf")))

    def recurse(pos, path_length, encountered_doors):
        if pos != start and board[pos].islower():
            dst = found_paths[board[pos]]
            dst[encountered_doors] = min(dst[encountered_doors], path_length)

        visited.add(pos)
        for i in adjacent(board, pos):
            if i not in visited:
                new_doors = encountered_doors
                if board[i].isupper():
                    new_doors |= frozenset([board[i]])
                recurse(i, path_length + 1, new_doors)
        visited.remove(pos)

    recurse(start, 0, frozenset())

    return found_paths


def compute_meta_graph(board, keys, starts):
    graph = {}
    for i in starts:
        graph[board[i]] = all_keys_from(board, i)
    for i in keys:
        graph[board[i]] = all_keys_from(board, i)
    return graph


def meta_graph_search(graph, num_keys):
    frontier = PriorityQueue()
    frontier.put((0, "@", frozenset()))
    visited = set()

    while frontier.qsize():
        dist, curr_node, openable_doors = frontier.get()
        # print(dist, curr_node, len(openable_doors))

        if len(openable_doors) == num_keys:
            # collected all keys!
            return dist

        if (curr_node, openable_doors) in visited:
            continue

        visited.add((curr_node, openable_doors))

        for next_node, poss_paths in graph[curr_node].items():
            if next_node.upper() in openable_doors:
                continue
            for required_keys, length in poss_paths.items():
                if required_keys <= openable_doors:
                    new_dist = dist + length
                    new_doors = openable_doors | frozenset([next_node.upper()])
                    if (next_node, new_doors) not in visited:
                        frontier.put((new_dist, next_node, new_doors))


def meta_graph_search_four(graph, num_keys):
    frontier = PriorityQueue()
    frontier.put((0, ("1", "2", "3", "4"), frozenset()))
    visited = set()

    while frontier.qsize():
        dist, nodes, openable_doors = frontier.get()
        # print(dist, nodes, len(openable_doors))

        if len(openable_doors) == num_keys:
            # collected all keys!
            return dist

        if (nodes, openable_doors) in visited:
            continue

        visited.add((nodes, openable_doors))

        for move_candidate in range(4):
            for next_node, poss_paths in graph[nodes[move_candidate]].items():
                if next_node.upper() in openable_doors:
                    continue
                for required_keys, length in poss_paths.items():
                    if required_keys <= openable_doors:
                        new_dist = dist + length
                        new_nodes = tuple(j if i != move_candidate else next_node for i, j in enumerate(nodes))
                        new_doors = openable_doors | frozenset([next_node.upper()])
                        if (new_nodes, new_doors) not in visited:
                            frontier.put((new_dist, new_nodes, new_doors))

# star 1
graph = compute_meta_graph(board, keys, [start])
print(meta_graph_search(graph, len(keys)))

# star 2
sx, sy = start
board[sx, sy] = "#"
board[sx, sy - 1] = "#"
board[sx, sy + 1] = "#"
board[sx - 1, sy] = "#"
board[sx + 1, sy] = "#"
board[sx - 1, sy - 1] = "1"
board[sx - 1, sy + 1] = "2"
board[sx + 1, sy - 1] = "3"
board[sx + 1, sy + 1] = "4"
graph = compute_meta_graph(board, keys, [(sx - 1, sy - 1), (sx - 1, sy + 1), (sx + 1, sy - 1), (sx + 1, sy + 1)])
print(meta_graph_search_four(graph, len(keys)))
