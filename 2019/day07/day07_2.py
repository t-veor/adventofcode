#!/usr/bin/env python3
import sys
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]

class IntCodeVM:
    def __init__(self, memory):
        self.memory = memory[:]
        self.pc = 0
        self.inputs = []
        self.outputs = []
        self.halted = False

    def push_input(self, x):
        self.inputs.append(x)

    def run(self, input_gen):
        buf = []

        def push_output(val):
            buf.append(val)

        while not self.halted:
            self.step(lambda: next(input_gen), push_output)
            yield from buf
            buf.clear()

    def read(self, loc):
        if 0 <= loc < len(self.memory):
            return self.memory[loc]
        else:
            return 0

    def fetch_decode(self):
        op = self.read(self.pc)
        opcode = op % 100
        src_mode = (op % 1000) // 100
        trg_mode = (op % 10000) // 1000
        dst_mode = op // 10000

        src = self.read(self.pc + 1)
        trg = self.read(self.pc + 2)
        dst = self.read(self.pc + 3)
        op1 = src if src_mode == 1 else self.read(src)
        op2 = trg if trg_mode == 1 else self.read(trg)
        op3 = dst if dst_mode == 1 else self.read(dst)

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
        else:
            raise Exception("Unknown opcode {}".format(opcode))


def run_amplifiers(program, settings):
    last_vm = None
    for i in settings:
        vm = IntCodeVM(program)
        if last_vm is None:
            last_vm = vm.run(iter([i, 0]))
        else:
            last_vm = vm.run(itertools.chain([i], last_vm))
    return next(last_vm)


def run_amplifiers_loop(program, settings):
    last_vm = None
    last_signal = 0

    def loop_gen():
        nonlocal last_signal
        while True:
            yield last_signal

    for i in settings:
        vm = IntCodeVM(program)
        if last_vm is None:
            last_vm = vm.run(itertools.chain([i], loop_gen()))
        else:
            last_vm = vm.run(itertools.chain([i], last_vm))

    try:
        while True:
            last_signal = next(last_vm)
    except StopIteration:
        return last_signal


# star 1
print(max(run_amplifiers(program, i) for i in itertools.permutations(range(5))))

# star 2
print(max(
    run_amplifiers_loop(program, i)
    for i in itertools.permutations(range(5, 10))))
