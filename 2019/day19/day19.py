#!/usr/bin/env python3
# pylint: disable=import-error
import sys
from os.path import dirname, join
sys.path.append(join(dirname(__file__), ".."))

from intcode import IntCodeVM
import cProfile

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]

def in_tractor_beam(x, y):
    vm = IntCodeVM(program)
    process = vm.run(iter((x, y)))
    return next(process)

def scan_area(program, width, height):
    count = 0
    for y in range(height):
        for x in range(width):
            val = in_tractor_beam(x, y)
            count += val
            """
            if val:
                print("#", end="")
            else:
                print(".", end="")
            """
        # print()

    return count


def find_square(program, width, height):
    MAX_INVERSE_SLOPE = 10
    last_x = 0
    y = 0
    while True:
        for x in range(last_x, last_x + MAX_INVERSE_SLOPE):
            if in_tractor_beam(x, y):
                # found the next line
                last_x = x

                if y - height + 1 > 0:
                    # try querying the other 3 points in the square
                    if (in_tractor_beam(x, y - height + 1)
                        and in_tractor_beam(x + width - 1, y - height + 1)
                        and in_tractor_beam(x + width - 1, y)):
                        return 10000 * x + (y - height + 1)
                break
        # print(y, last_x)
        y += 1


# star 1
print(scan_area(program, 50, 50))

# star 2
print(find_square(program, 100, 100))
