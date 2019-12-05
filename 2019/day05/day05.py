#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [int(i) for i in open(filename).read().split(",")]

def run_program(memory, inputs):
    memory = memory[:]
    inputs = iter(inputs)
    outputs = []

    read = lambda i: memory[i] if len(memory) > i else 0

    pc = 0
    while True:
        # decode
        op = read(pc)
        opcode = op % 100
        src_mode = (op % 1000) // 100
        trg_mode = (op % 10000) // 1000
        dst_mode = op // 10000

        src, trg, dst = read(pc + 1), read(pc + 2), read(pc + 3)
        op1 = src if src_mode == 1 else read(src)
        op2 = trg if trg_mode == 1 else read(trg)
        op3 = dst if dst_mode == 1 else read(dst)

        if opcode == 99:
            break
        elif opcode == 1:
            memory[dst] = op1 + op2
            pc += 4
        elif opcode == 2:
            memory[dst] = op1 * op2
            pc += 4
        elif opcode == 3:
            try:
                memory[src] = next(inputs)
            except StopIteration:
                memory[src] = 0
            pc += 2
        elif opcode == 4:
            outputs.append(op1)
            pc += 2
        elif opcode == 5:
            if op1 != 0:
                pc = op2
            else:
                pc += 3
        elif opcode == 6:
            if op1 == 0:
                pc = op2
            else:
                pc += 3
        elif opcode == 7:
            memory[dst] = int(op1 < op2)
            pc += 4
        elif opcode == 8:
            memory[dst] = int(op1 == op2)
            pc += 4
        else:
            raise Exception("Unknown opcode {}".format(op))

    # print(memory)
    return outputs

# star 1
print(run_program(data, [1])[-1])

# star 2
print(run_program(data, [5])[0])
