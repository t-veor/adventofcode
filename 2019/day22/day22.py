#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

instrs = []
for i in open(filename).read().splitlines():
    if i.startswith("deal into new stack"):
        instrs.append(("rev", 0))
    elif i.startswith("cut "):
        instrs.append(("cut", int(i[len("cut "):])))
    elif i.startswith("deal with increment "):
        instrs.append(("deal", int(i[len("deal with increment "):])))
    else:
        raise "Unknown instruction: " + i


deck = list(range(10007))

for op, num in instrs:
    if op == "rev":
        deck = list(reversed(deck))
    elif op == "cut":
        deck = deck[num:] + deck[:num]
    elif op == "deal":
        new_deck = [0] * len(deck)
        for i, j in enumerate(deck):
            new_deck[i * num % len(new_deck)] = j
        deck = new_deck

# star 1
print(deck.index(2019))

# star 2
# the input actually represents an affine transformation of card num -> position
# construct the map x -> (coeff * x + offset) % ncards
ncards = 119315717514047
coeff, offset = 1, 0

for op, num in instrs:
    if op == "rev":
        # x -> -1 - x
        # -1 - (coeff * x + offset) = -coeff * x + -1 - offset
        coeff = (-coeff) % ncards
        offset = (-1 - offset) % ncards
    elif op == "cut":
        # x -> x - num
        # (coeff * x + offset) - num = coeff * x + offset - num
        offset = (offset - num) % ncards
    elif op == "deal":
        # x -> num * x
        # num * (coeff * x + offset) = num * coeff * x + num * offset
        coeff = (num * coeff) % ncards
        offset = (num * offset) % ncards

# figure out the final transformation after applying n iterations
niter = 101741582076661


def repeated_apply(transform, base, n):
    if n == 0:
        return (1, 0)
    elif n % 2 == 0:
        # square the transform
        coeff, offset = repeated_apply(transform, base, n // 2)
        # coeff * (coeff * x + offset) + offset = coeff**2 * x + coeff * offset + offset
        coeff, offset = (coeff * coeff) % base, (coeff * offset + offset) % base
        return (coeff, offset)
    else:
        # apply the transform once
        coeff, offset = repeated_apply(transform, base, n - 1)
        coeff = (coeff * transform[0]) % base
        offset = (offset * transform[0] + transform[1]) % base
        return (coeff, offset)


final_transform = repeated_apply((coeff, offset), ncards, niter)


def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, x, y = egcd(b % a, a)
        return (g, y - (b // a) * x, x)


inverse_coeff = egcd(final_transform[0], ncards)[1] % ncards
inverse_offset = -final_transform[1]
print((inverse_coeff * (2020 + inverse_offset)) % ncards)
