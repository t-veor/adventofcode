#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

lines = open(filename).read().splitlines()
split_index = lines.index("")
crate_lines = lines[:split_index - 1]

stack_count = (len(lines[split_index - 1]) + 1) // 4
stacks = [[] for _ in range(stack_count)]
for i in range(split_index - 2, -1, -1):
    line = lines[i]
    for i in range(stack_count):
        if line[4 * i + 1].isalpha():
            stacks[i].append(line[4 * i + 1])

instructions = []
for line in lines[split_index + 1:]:
    split_line = line.split()
    instructions.append((int(split_line[1]), int(split_line[3]), int(split_line[5])))

# star 1
rearranged_stacks = [i[:] for i in stacks]
for count, src, dst in instructions:
    for _ in range(count):
        rearranged_stacks[dst - 1].append(rearranged_stacks[src - 1].pop())
print("".join(stack[-1] for stack in rearranged_stacks))

# star 2
rearranged_stacks = [i[:] for i in stacks]
for count, src, dst in instructions:
    rearranged_stacks[dst - 1].extend(rearranged_stacks[src - 1][-count:])
    del rearranged_stacks[src - 1][-count:]
print("".join(stack[-1] for stack in rearranged_stacks))
