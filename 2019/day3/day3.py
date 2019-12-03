
#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

wires = open(filename).read().splitlines()
for i in range(len(wires)):
    wires[i] = [(j[0], int(j[1:])) for j in wires[i].split(",")]

# process wires into a list of horizontal and vertical lines
# keep track of the starting points for step calculations later
lines = []
for i in wires:
    l = []
    x, y = 0, 0
    for dir, dist in i:
        if dir == "R":
            l.append(("h", (x, y, dist), (x, y)))
            x += dist
        elif dir == "L":
            l.append(("h", (x - dist, y, dist), (x, y)))
            x -= dist
        elif dir == "D":
            l.append(("v", (x, y, dist), (x, y)))
            y += dist
        elif dir == "U":
            l.append(("v", (x, y - dist, dist), (x, y)))
            y -= dist
    lines.append(l)

def intersects(l1, l2):
    dir1, (x1, y1, dist1), _ = l1
    dir2, (x2, y2, dist2), _ = l2

    if dir1 == dir2:
        # assume overlapping wires don't count?
        return None
    else:
        # different directions
        if dir1 == "h":
            if x1 <= x2 <= x1 + dist1 and y2 <= y1 <= y2 + dist2:
                return (x2, y1)
        elif dir1 == "v":
            if y1 <= y2 <= y1 + dist1 and x2 <= x1 <= x2 + dist2:
                return (x1, y2)

        return None

# star1
min_dist = 0
for i in lines[0]:
    for j in lines[1]:
        intersection = intersects(i, j)
        if intersection is not None:
            dist = abs(intersection[0]) + abs(intersection[1])
            if min_dist == 0 or dist > 0 and dist < min_dist:
                min_dist = dist
print(min_dist)

# star2
min_steps = 0
s1 = 0
for i in lines[0]:
    s2 = 0
    for j in lines[1]:
        intersection = intersects(i, j)
        if intersection is not None:
            x, y = intersection
            _, _, (x1, y1) = i
            _, _, (x2, y2) = j
            steps = s1 + s2 + abs(x - x1) + abs(x - x2) + abs(y - y1) + abs(y - y2)
            if min_steps == 0 or steps > 0 and steps < min_steps:
                min_steps = steps
        _, (_, _, dist2), _ = j
        s2 += dist2
    _, (_, _, dist1), _ = i
    s1 += dist1
print(min_steps)
