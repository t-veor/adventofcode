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


def run_robot(program, start=0):
    board = defaultdict(lambda: 0)
    painted = set()
    x, y = 0, 0
    board[x, y] = start
    dx, dy = 0, -1
    vm = IntCodeVM(program)

    def camera_out():
        while True:
            yield board[x, y]

    process = vm.run(camera_out())

    try:
        while True:
            board[x, y] = next(process)
            painted.add((x, y))
            turn = next(process)
            if turn == 0:
                dx, dy = dy, -dx
            else:
                dx, dy = -dy, dx
            x += dx
            y += dy
    except StopIteration:
        pass

    return board, painted


def print_board(board):
    max_x = max(i[0] for i in board.keys())
    min_x = min(i[0] for i in board.keys())
    max_y = max(i[1] for i in board.keys())
    min_y = min(i[1] for i in board.keys())

    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            print("\u2588" if board[x, y] else " ", end="")
        print()

# star 1
print(len(run_robot(program)[1]))

# star 2
print_board(run_robot(program, start=1)[0])
