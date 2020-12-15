#!/usr/bin/env python3
import sys
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


data = open(filename).read().splitlines()
instructions = []
for line in data:
    if line.startswith("mask = "):
        instructions.append(("mask", line[len("mask = "):]))
    else:
        dst, imm = line[len("mem["):].split("] = ")
        instructions.append(("mem", int(dst), int(imm)))


def get_bitmasks(mask):
    and_mask = (1 << 36) - 1
    or_mask = 0

    for i, j in enumerate(mask):
        if j == "0":
            and_mask ^= 1 << (35 - i)
        elif j == "1":
            or_mask |= 1 << (35 - i)

    return and_mask, or_mask


def address_bitmask_iterator(mask, addr):
    x_count = mask.count("X")
    all_bits = (1 << 36) - 1
    for xs in itertools.product((0, 1), repeat=x_count):
        x_index = 0
        new_addr = addr
        for i, j in enumerate(mask):
            if j == "1":
                new_addr |= 1 << (35 - i)
            elif j == "X":
                x = xs[x_index]
                x_index += 1
                if x:
                    new_addr |= 1 << (35 - i)
                else:
                    new_addr &= all_bits ^ (1 << (35 - i))
        yield new_addr


# star 1
and_mask, or_mask = 0, 0
mem = {}
for instr in instructions:
    if instr[0] == "mask":
        and_mask, or_mask = get_bitmasks(instr[1])
    else:
        value = (instr[2] & and_mask) | or_mask
        mem[instr[1]] = value
print(sum(mem.values()))

# star 2
mem = {}
mask = ""
for instr in instructions:
    if instr[0] == "mask":
        mask = instr[1]
    else:
        addr, data = instr[1], instr[2]
        for a in address_bitmask_iterator(mask, addr):
            mem[a] = data
print(sum(mem.values()))
