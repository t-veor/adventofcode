#!/usr/bin/env python3
import sys
import numpy as np

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().strip()
base, rules = data.split("\n\n")
rules = [tuple(i.split(" -> ")) for i in rules.splitlines()]


def make_index_mapping(base, rules):
    letters = set()
    for c in base:
        letters.add(c)
    for (a, b), c in rules:
        letters.add(a)
        letters.add(b)
        letters.add(c)
    letters = sorted(letters)
    letters.append("$")

    mapping = {}
    n = 0
    for i in letters:
        for j in letters:
            mapping[i + j] = n
            n += 1
    return mapping


def polymer_to_vector(polymer, mapping):
    v = np.zeros((len(mapping),), dtype=np.int64)
    polymer = f"${polymer}$"
    for i in range(len(polymer) - 1):
        v[mapping[polymer[i:i+2]]] += 1
    return v


def rules_to_matrix(rules, mapping):
    m = np.zeros((len(mapping), len(mapping)), dtype=np.int64)

    identities = set(mapping.keys())

    for original, result in rules:
        product_one = original[0] + result
        product_two = result + original[1]

        m[mapping[product_one], mapping[original]] += 1
        m[mapping[product_two], mapping[original]] += 1

        identities.remove(original)

    for id in identities:
        m[mapping[id], mapping[id]] += 1

    return m


def vector_to_freqs(vector, mapping):
    # make an inverse mapping
    originals = [""] * len(mapping)
    for i, j in mapping.items():
        originals[j] = i

    counts = {}
    for i, j in zip(vector, originals):
        a, b = j
        counts[a] = counts.get(a, 0) + i
        counts[b] = counts.get(b, 0) + i

    del counts["$"]

    for i in counts:
        counts[i] //= 2

    return counts


mapping = make_index_mapping(base, rules)
v = polymer_to_vector(base, mapping)
m = rules_to_matrix(rules, mapping)

# star 1
for i in range(10):
    v = m.dot(v)
freqs = vector_to_freqs(v, mapping)
counts = sorted(freqs.values())
print(counts[-1] - counts[0])

# star 2
# we could implement the O(log(n)) matrix exponentiation algorithm but the
# exponent is only 40, why bother
for i in range(30):
    v = m.dot(v)
freqs = vector_to_freqs(v, mapping)
counts = sorted(freqs.values())
print(counts[-1] - counts[0])
