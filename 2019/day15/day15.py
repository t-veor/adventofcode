#!/usr/bin/env python3
import sys
import itertools
from collections import defaultdict, deque

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


def adjacent(pos):
    x, y = pos
    yield 1, (x, y - 1)
    yield 2, (x, y + 1)
    yield 3, (x - 1, y)
    yield 4, (x + 1, y)


OPPOSITE = { 1: 2, 2: 1, 3: 4, 4: 3 }


def bot_dfs(program):
    vm = IntCodeVM(program)
    next_command = 0
    def get_next_command():
        while True:
            yield next_command
    process = vm.run(get_next_command())

    visited = set()
    board = {}
    dists = {}
    oxygen_location = None

    def recurse(pos, n):
        nonlocal next_command, oxygen_location
        visited.add(pos)
        for command, new_pos in adjacent(pos):
            if new_pos not in visited:
                # try moving to new_pos
                next_command = command
                bot_status = next(process)

                board[new_pos] = bot_status
                if bot_status == 0:
                    # hit a wall
                    continue
                elif bot_status == 1:
                    pass
                else:
                    # found the oxygen system
                    oxygen_location = new_pos
                dists[new_pos] = n + 1

                recurse_status = recurse(new_pos, n + 1)
                if recurse_status is not None:
                    return recurse_status

                # move back
                next_command = OPPOSITE[command]
                next(process)

    board[0, 0] = 1
    recurse((0, 0), 0)

    return board, oxygen_location, dists[oxygen_location]


def oxygen_bfs(board, starting_pos):
    visited = set()
    frontier = deque()
    max_dist = 0
    frontier.append((starting_pos, 0))
    visited.add(starting_pos)

    while frontier:
        pos, dist = frontier.popleft()
        max_dist = max(dist, max_dist)
        for _, new_pos in adjacent(pos):
            if board.get(new_pos, 0) != 0 and new_pos not in visited:
                visited.add(new_pos)
                frontier.append((new_pos, dist + 1))

    return max_dist



board, oxygen_location, oxygen_dist = bot_dfs(program)

# star 1
print(oxygen_dist)

# star 2
print(oxygen_bfs(board, oxygen_location))
