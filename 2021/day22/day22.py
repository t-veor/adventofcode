#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


class Cuboid:
    def __init__(self, x_range, y_range, z_range):
        self.x_range = x_range
        self.y_range = y_range
        self.z_range = z_range

    def __repr__(self):
        x = f"x={self.x_range[0]}..{self.x_range[1]}"
        y = f"x={self.y_range[0]}..{self.y_range[1]}"
        z = f"x={self.z_range[0]}..{self.z_range[1]}"
        return f"{x},{y},{z}"

    def volume(self):
        width = self.x_range[1] - self.x_range[0] + 1
        height = self.y_range[1] - self.y_range[0] + 1
        depth = self.z_range[1] - self.z_range[0] + 1
        return width * height * depth

    def restrict(self):
        return self.intersect(Cuboid((-50, 50), (-50, 50), (-50, 50)))

    def is_valid(self):
        if self.x_range[1] < self.x_range[0]:
            return False

        if self.y_range[1] < self.y_range[0]:
            return False

        if self.z_range[1] < self.z_range[0]:
            return False

        return True

    def __eq__(self, other):
        return self.x_range == other.x_range and self.y_range == other.y_range and self.z_range == other.z_range

    def __hash__(self):
        return hash((self.x_range, self.y_range, self.z_range))

    def intersect(self, other):
        min_x = max(self.x_range[0], other.x_range[0])
        max_x = min(self.x_range[1], other.x_range[1])
        min_y = max(self.y_range[0], other.y_range[0])
        max_y = min(self.y_range[1], other.y_range[1])
        min_z = max(self.z_range[0], other.z_range[0])
        max_z = min(self.z_range[1], other.z_range[1])

        cube = Cuboid((min_x, max_x), (min_y, max_y), (min_z, max_z))
        if cube.is_valid():
            return cube
        else:
            return None


instrs = []
with open(filename) as f:
    for line in f:
        line = line.strip()
        if line:
            instr, ranges = line.split(" ")
            instr = 1 if instr == "on" else -1
            ranges = ranges.split(",")
            ranges = tuple(tuple(int(j) for j in i[2:].split("..")) for i in ranges)
            instrs.append((instr, Cuboid(*ranges)))


def process_instrs(instrs):
    cuboids = {}

    for sign, cuboid in instrs:
        new_cuboids = []

        for existing_cuboid, count in cuboids.items():
            intersection = cuboid.intersect(existing_cuboid)
            if intersection is not None:
                new_cuboids.append((intersection, -count))
        if sign > 0:
            new_cuboids.append((cuboid, sign))

        for new_cuboid, count in new_cuboids:
            cuboids[new_cuboid] = cuboids.get(new_cuboid, 0) + count

    total_volume = 0
    for cuboid, count in cuboids.items():
        total_volume += cuboid.volume() * count
    return total_volume


# star 1
initialization_instrs = [(sign, cuboid.restrict()) for sign, cuboid in instrs]
initialization_instrs = [(sign, cuboid) for sign, cuboid in initialization_instrs if cuboid is not None]
print(process_instrs(initialization_instrs))

# star 2
print(process_instrs(instrs))
