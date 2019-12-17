#!/usr/bin/env python3
from collections import defaultdict

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
        opcode, (src, _trg, dst), (op1, op2, _op3) = self.fetch_decode()

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
