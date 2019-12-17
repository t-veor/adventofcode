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

# star 1
vm = IntCodeVM(program)
print(next(vm.run(iter([1]))))

# star 2
vm = IntCodeVM(program)
print(next(vm.run(iter([2]))))
