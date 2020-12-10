#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

instructions = []
for i in open(filename).read().splitlines():
    op, imm = i.split(" ")
    instructions.append((op, int(imm)))


def exec(instructions):
    executed = set()
    pc = 0
    acc = 0
    while pc < len(instructions):
        if pc in executed:
            return False, acc
        executed.add(pc)

        op, imm = instructions[pc]
        if op == "nop":
            pass
        elif op == "acc":
            acc += imm
        elif op == "jmp":
            pc += imm
            continue
        else:
            raise "unreachable"
        pc += 1
    return True, acc


# star 1
print(exec(instructions)[1])

# star 2
for i, (op, imm) in enumerate(instructions):
    if op == "nop" or op == "jmp":
        new_instructions = instructions[:]
        new_instructions[i] = ("nop" if op == "jmp" else "jmp", imm)
        terminated, acc = exec(new_instructions)
        if terminated:
            print(acc)
            break
