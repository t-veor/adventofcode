#!/usr/bin/env python3
# pylint: disable=import-error
import sys
from os.path import dirname, join
sys.path.append(join(dirname(__file__), ".."))

from intcode import IntCodeVM
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]


def stdin_gen():
    while True:
        yield ord(sys.stdin.read(1))

commands = """north
north
take manifold
west
south
east
south
take fixed point
north
west
north
east
south
west
take boulder
east
south
west
south
take polygon
north
east
north
north
west
north
east
east
north
"""
command_iter = iter(ord(i) for i in commands)

vm = IntCodeVM(program)
for i in vm.run(command_iter):
    print(chr(i), end="")
