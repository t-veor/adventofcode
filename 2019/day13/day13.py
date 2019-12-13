#!/usr/bin/env python3
import sys
import itertools
from collections import defaultdict

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]

class IntCodeVM:
    def __init__(self, memory):
        self.memory = defaultdict(lambda: 0, enumerate(memory))
        self.pc = 0
        self.halted = False
        self.rbase = 0

    def run(self, input_gen):
        buf = []

        def push_output(val):
            buf.append(val)

        while not self.halted:
            self.step(lambda: next(input_gen), push_output)
            yield from buf
            buf.clear()

    def read(self, loc):
        return self.memory[loc]

    def fetch_decode(self):
        op = self.read(self.pc)
        opcode = op % 100
        src_mode = (op % 1000) // 100
        trg_mode = (op % 10000) // 1000
        dst_mode = op // 10000

        def op_decode(mode, param):
            if mode == 0:
                return param, self.read(param)
            elif mode == 1:
                return 0, param
            else:
                # mode == 2
                return param + self.rbase, self.read(param + self.rbase)

        src = self.read(self.pc + 1)
        trg = self.read(self.pc + 2)
        dst = self.read(self.pc + 3)
        src, op1 = op_decode(src_mode, src)
        trg, op2 = op_decode(trg_mode, trg)
        dst, op3 = op_decode(dst_mode, dst)

        return opcode, (src, trg, dst), (op1, op2, op3)

    def step(self, get_input, push_output):
        opcode, (src, trg, dst), (op1, op2, op3) = self.fetch_decode()

        if opcode == 99:
            self.halted = True
            return
        elif opcode == 1:
            self.memory[dst] = op1 + op2
            self.pc += 4
        elif opcode == 2:
            self.memory[dst] = op1 * op2
            self.pc += 4
        elif opcode == 3:
            self.memory[src] = get_input()
            self.pc += 2
        elif opcode == 4:
            push_output(op1)
            self.pc += 2
        elif opcode == 5:
            if op1 != 0:
                self.pc = op2
            else:
                self.pc += 3
        elif opcode == 6:
            if op1 == 0:
                self.pc = op2
            else:
                self.pc += 3
        elif opcode == 7:
            self.memory[dst] = int(op1 < op2)
            self.pc += 4
        elif opcode == 8:
            self.memory[dst] = int(op1 == op2)
            self.pc += 4
        elif opcode == 9:
            self.rbase += op1
            self.pc += 2
        else:
            raise Exception("Unknown opcode {}".format(opcode))


def sgn(x):
    if x == 0:
        return 0
    elif x < 0:
        return -1
    else:
        return 1


def run_game(program):
    program[0] = 2
    # screen = {}
    score = 0
    vm = IntCodeVM(program)
    max_x = 0
    max_y = 0
    ball_x = 0
    paddle_x = 0

    def get_input():
        while True:
            """
            for y in range(max_y + 1):
                for x in range(max_x + 1):
                    tile = screen.get((x, y), 0)
                    if tile == 0:
                        p = " "
                    elif tile == 1:
                        p = "\u2588"
                    elif tile == 2:
                        p = "\u2592"
                    elif tile == 3:
                        p = "-"
                    else:
                        p = "o"
                    print(p, end="")
                print()
            print("Score: {}".format(score))
            """
            yield sgn(ball_x - paddle_x)

    process = vm.run(get_input())

    try:
        while True:
            x, y, tile = next(process), next(process), next(process)
            if x == -1 and y == 0:
                score = tile
            else:
                max_x = max(x, max_x)
                max_y = max(y, max_y)
                # screen[x, y] = tile

                if tile == 3:
                    paddle_x = x
                elif tile == 4:
                    ball_x = x
    except StopIteration:
        pass

    return score


# star 1
vm = IntCodeVM(program)
blocks = set()
try:
    process = vm.run(iter(()))
    while True:
        x, y, id = next(process), next(process), next(process)
        if id == 2:
            blocks.add((x, y))
except StopIteration:
    pass
print(len(blocks))

# star 2
print(run_game(program))
