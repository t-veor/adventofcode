#!/usr/bin/env python3
import sys
from collections import deque, defaultdict

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

pairs = [tuple(i.split(")")) for i in open(filename).read().splitlines()]

orbiting = {j: i for i, j in pairs}

graph = defaultdict(lambda: [])
for i, j in pairs:
    graph[i].append(j)
    graph[j].append(i)

def bfs(graph, start, target):
    frontier = deque()
    visited = set()
    frontier.append((start, 0))
    visited.add(start)

    while frontier:
        node, d = frontier.popleft()
        if node == target:
            return d
        for i in graph[node]:
            if i not in visited:
                frontier.append((i, d + 1))
                visited.add(i)

    return -1

# star 1
count = 0
for i in orbiting:
    while i in orbiting:
        count += 1
        i = orbiting[i]
print(count)

# star 2
print(bfs(graph, orbiting["YOU"], orbiting["SAN"]))
