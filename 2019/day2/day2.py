#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [int(i) for i in open(filename).read().split(",")]

def run_program(memory, arg1, arg2):
    memory = memory[:]
    memory[1] = arg1
    memory[2] = arg2

    pc = 0
    while True:
        op = memory[pc]
        if op == 99:
            break
        else:
            src, dst, trg = memory[pc+1:pc+4]
            operand1, operand2 = memory[src], memory[dst]
            if op == 1:
                result = operand1 + operand2
            elif op == 2:
                result = operand1 * operand2
            else:
                raise Exception("Unknown opcode {}".format(op))
            memory[trg] = result
        pc += 4
    # print(memory)
    return memory[0]

# star1
print(run_program(data, 12, 2))

# star2
target = 19690720
for noun in range(100):
    for verb in range(100):
        if run_program(data, noun, verb) == target:
            print(100 * noun + verb)
            sys.exit()
