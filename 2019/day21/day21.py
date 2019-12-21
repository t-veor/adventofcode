#!/usr/bin/env python3
# pylint: disable=import-error
import sys
from os.path import dirname, join
sys.path.append(join(dirname(__file__), ".."))

from intcode import IntCodeVM

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]


def run_springbot(gen, suppress_ascii=False):
    vm = IntCodeVM(program)
    for i in vm.run(gen):
        if i > 127:
            print(i)
        elif not suppress_ascii:
            print(chr(i), end="")


# star 1
# (!A || !B || !C)    &&  D
# (if there's a hole) AND (we can land)
star1_prog = """NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
"""
run_springbot((ord(i) for i in star1_prog), True)

# star 2
# (!A || !B || !C)    &&   D            &&  (E || H)
# (if there's a hole) AND (we can land) AND (we can either walk or jump immediately again)
star2_prog = """NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT H T
NOT T T
OR E T
AND T J
RUN
"""
run_springbot((ord(i) for i in star2_prog), True)
