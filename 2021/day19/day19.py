#!/usr/bin/env python3
import sys
import numpy as np
from collections import Counter, deque

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

scanner_data = open(filename).read().strip().split("\n\n")
scanners = [
    [
        np.array(
            [int(k) for k in j.split(",")], dtype=np.int32
        ) for j in i.splitlines()[1:]
    ] for i in scanner_data
]


def generate_rotation_matrices():
    rx = np.array([
        [ 1,  0,  0],
        [ 0,  0, -1],
        [ 0,  1,  0],
    ], dtype=np.int32)
    ry = np.array([
        [ 0,  0, -1],
        [ 0,  1,  0],
        [ 1,  0,  0],
    ], dtype=np.int32)
    rz = np.array([
        [ 0, -1,  0],
        [ 1,  0,  0],
        [ 0,  0,  1],
    ], dtype=np.int32)
    # I can't be bothered to work out the group structure of this rotation
    # group, but clearly {rx, ry, rz} must generate it, so here we go:
    rotations_set = set()
    rotations = []

    to_process = deque()
    to_process.append(np.identity(3, dtype=np.int32))
    while to_process:
        rot = to_process.popleft()
        flattened_rot = tuple(rot.ravel())
        if flattened_rot in rotations_set:
            continue

        rotations_set.add(flattened_rot)
        rotations.append(rot)

        for r in (rx, ry, rz):
            to_process.append(r.dot(rot))

    assert len(rotations) == 24
    return rotations


ROTATION_MATRICES = generate_rotation_matrices()


def get_rotation_and_offset(data0, data1):
    for r in ROTATION_MATRICES:
        offsets = Counter()

        for i in data1:
            v = r.dot(i)
            for u in data0:
                diff = tuple(u - v)
                offsets[diff] += 1

        offset, freq = offsets.most_common(1)[0]
        if freq >= 12:
            # found it
            return np.array(offset, dtype=np.int32), r

    # didn't find any rotation that worked?
    return None


def get_scanner_offsets(scanners):
    to_process = deque()
    to_process.append(0)

    offset_map = {
        0: (
            np.array([0, 0, 0], dtype=np.int32),
            np.identity(3, dtype=np.int32)
        )
    }

    while to_process:
        i = to_process.popleft()

        parent_offset, parent_rotation = offset_map[i]
        transformed_beacons = [
            parent_offset + parent_rotation.dot(v) for v in scanners[i]
        ]

        for j in range(len(scanners)):
            if j in offset_map:
                continue

            result = get_rotation_and_offset(transformed_beacons, scanners[j])
            if result is not None:
                offset, rotation = result
                offset_map[j] = (offset, rotation)
                to_process.append(j)

    offsets = []
    for i in range(len(scanners)):
        if i not in offset_map:
            raise f"Missed scanner {i} during discovery process!"
        offsets.append(offset_map[i])

    return offsets


def transform_and_squash_points(scanners, offsets):
    final_points = set()

    for untransformed_points, (offset, rotation) in zip(scanners, offsets):
        for u in untransformed_points:
            v = rotation.dot(u) + offset
            final_points.add(tuple(v))

    return [np.array(i, dtype=np.int32) for i in final_points]


offsets = get_scanner_offsets(scanners)
points = transform_and_squash_points(scanners, offsets)

# star 1
print(len(points))

# star 2
max_distance = 0
for i in range(len(offsets)):
    for j in range(i + 1, len(offsets)):
        u = offsets[i][0]
        v = offsets[j][0]
        distance = np.sum(np.absolute(u - v))
        max_distance = max(distance, max_distance)
print(max_distance)
