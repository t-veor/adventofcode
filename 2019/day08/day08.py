#!/usr/bin/env python3
import sys
from collections import Counter

IMAGE_SIZE = (25, 6)

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().strip()
size = IMAGE_SIZE[0] * IMAGE_SIZE[1]
layers = [data[i:i+size] for i in range(0, len(data), size)]

freqs = [Counter(j for j in i) for i in layers]

# star 1
fewest_zeroes = min(freqs, key=lambda i: i["0"])
print(fewest_zeroes["1"] * fewest_zeroes["2"])

# star 2
image = ["2"] * size
for i in range(size):
    for j in layers:
        if image[i] == "2":
            image[i] = j[i]
        else:
            break
print(
    "\n".join(
        "".join("\u2588" if j == "1" else " " for j in image[i:i+IMAGE_SIZE[0]])
        for i in range(0, len(image), IMAGE_SIZE[0])
    )
)
