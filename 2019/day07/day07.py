#!/usr/bin/env python3
import sys
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [int(i) for i in open(filename).read().split(",")]

class IntCodeVM:
    def __init__(self, memory):
        self.memory = memory[:]
        self.pc = 0
        self.inputs = []
        self.outputs = []
        self.halted = False

    def push_input(self, x):
        self.inputs.append(x)

    def run_until_output(self):
        while not self.halted and not self.outputs:
            self.step()

        if self.outputs:
            return self.outputs.pop(0)

    def step(self):
        if self.halted:
            return

        read = lambda i: self.memory[i] if len(self.memory) > i else 0

        op = read(self.pc)
        opcode = op % 100
        src_mode = (op % 1000) // 100
        trg_mode = (op % 10000) // 1000
        dst_mode = op // 10000

        src, trg, dst = read(self.pc + 1), read(self.pc + 2), read(self.pc + 3)
        op1 = src if src_mode == 1 else read(src)
        op2 = trg if trg_mode == 1 else read(trg)
        op3 = dst if dst_mode == 1 else read(dst)

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
            self.memory[src] = self.inputs.pop(0)
            self.pc += 2
        elif opcode == 4:
            self.outputs.append(op1)
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
            raise Exception("Unknown opcode {}".format(op))

def run_amplifiers(program, settings):
    curr_input = 0
    for i in settings:
        vm = IntCodeVM(program)
        vm.push_input(i)
        vm.push_input(curr_input)
        curr_input = vm.run_until_output()
    return curr_input

# star1
max_signal = 0
for i in itertools.permutations(range(5)):
    signal = run_amplifiers(data, i)
    if signal > max_signal:
        max_signal = signal
print(max_signal)

def run_amplifiers_loop(program, settings):
    vms = []
    for i in settings:
        vm = IntCodeVM(program)
        vm.push_input(i)
        vms.append(vm)

    last_signal = 0
    curr_signal = 0
    while True:
        for i in range(len(vms)):
            vms[i].push_input(curr_signal)
            curr_signal = vms[i].run_until_output()
            if curr_signal == None:
                return last_signal
        last_signal = curr_signal
    return last_signal

# star 2
max_signal = 0
for i in itertools.permutations(range(5, 10)):
    signal = run_amplifiers_loop(data, i)
    if signal > max_signal:
        max_signal = signal
print(max_signal)
