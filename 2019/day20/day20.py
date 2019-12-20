#!/usr/bin/env python3
import sys
from collections import defaultdict, deque

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


spaces = set()
letters = {}
portals = defaultdict(lambda: [])
max_x = 0
for y, l in enumerate(open(filename).read().splitlines()):
    for x, c in enumerate(l):
        if x > max_x:
            max_x = x
        if c == ".":
            spaces.add((x, y))
        elif c.isalpha():
            letters[x, y] = c
            # check to see if there is an adjacent letter
            if letters.get((x - 1, y), "").isalpha():
                other = (x - 1, y)
            elif letters.get((x, y - 1), "").isalpha():
                other = (x, y - 1)
            else:
                continue
            portal = letters[other] + c
            portals[portal].append((other, (x, y)))
width, height = max_x, y


def adjacent(pos):
    x, y = pos
    yield x - 1, y
    yield x + 1, y
    yield x, y - 1
    yield x, y + 1


links = {}
portal_name = {}
start, end = None, None
for i in portals:
    portal_positions = portals[i]
    adjacent_spaces = []
    for position in portal_positions:
        for letter_pos in position:
            for adj in adjacent(letter_pos):
                if adj in spaces:
                    adjacent_spaces.append(adj)
    if i == "AA":
        start = adjacent_spaces[0]
    elif i == "ZZ":
        end = adjacent_spaces[0]
    else:
        assert(len(adjacent_spaces) == 2)
        links[adjacent_spaces[0]] = adjacent_spaces[1]
        links[adjacent_spaces[1]] = adjacent_spaces[0]
        portal_name[adjacent_spaces[0]] = i
        portal_name[adjacent_spaces[1]] = i

links_labelled = {}
for i in links:
    x, y = i
    # check if it's on the outer circle
    if x == 2 or x == width - 2 or y == 2 or y == height - 2:
        links_labelled[i] = (-1, links[i])
    else:
        links_labelled[i] = (1, links[i])


def bfs(spaces, links, start, end):
    frontier = deque()
    visited = set()
    frontier.append((0, start))
    visited.add(start)

    while frontier:
        dist, pos = frontier.popleft()
        if pos == end:
            return dist

        for adj in adjacent(pos):
            if adj in spaces and adj not in visited:
                visited.add(adj)
                frontier.append((dist + 1, adj))
        if pos in links:
            linked = links[pos]
            if linked not in visited:
                visited.add(linked)
                frontier.append((dist + 1, linked))


def bfs_levels(spaces, links, start, end):
    frontier = deque()
    visited = set()
    frontier.append((0, 0, start))
    visited.add((0, start))

    while frontier:
        dist, level, pos = frontier.popleft()
        if level == 0 and pos == end:
            return dist

        for adj in adjacent(pos):
            if adj in spaces and (level, adj) not in visited:
                visited.add((level, adj))
                frontier.append((dist + 1, level, adj))
        if pos in links:
            ld, linked = links[pos]
            if level + ld >= 0 and (level + ld, linked) not in visited:
                visited.add((level + ld, linked))
                frontier.append((dist + 1, level + ld, linked))


# star 1
print(bfs(spaces, links, start, end))

# star 2
print(bfs_levels(spaces, links_labelled, start, end))
