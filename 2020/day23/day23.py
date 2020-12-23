#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

starting_cups = [int(i) for i in open(filename).read().splitlines()[0]]


# use a linked list to create the cyclic structure
def cyclic_cup_list(cups):
    cup_list = {}
    for i, j in enumerate(cups):
        next_cup = cups[0] if i == len(cups) - 1 else cups[i+1]
        cup_list[j] = next_cup
    return cup_list


def cyclic_cup_list_fill(starting_cups, max_cup):
    cup_list = {}
    max_starting_cup = max(starting_cups)
    for i, j in enumerate(starting_cups):
        next_cup = max_starting_cup + 1 if i == len(starting_cups) - 1 else starting_cups[i + 1]
        cup_list[j] = next_cup
    for i in range(max_starting_cup + 1, max_cup + 1):
        next_cup = starting_cups[0] if i == max_cup else i + 1
        cup_list[i] = next_cup
    return cup_list


def simulate(cup_list, min_cup, max_cup, starting_cup, moves):
    current_cup = starting_cup
    for _ in range(moves):
        # grab the next three cups after the current cup
        a = cup_list[current_cup]
        b = cup_list[a]
        c = cup_list[b]
        # remove them from the list
        cup_list[current_cup] = cup_list[c]
        # determine the next cup
        next_cup = current_cup
        while True:
            next_cup -= 1
            if next_cup < min_cup:
                next_cup = max_cup
            if next_cup not in (a, b, c):
                break
        # insert the three cups back in at next_cup
        cup_list[c] = cup_list[next_cup]
        cup_list[next_cup] = a
        # advance the current cup
        current_cup = cup_list[current_cup]

    return cup_list


def cup_walk(cup_list, starting_cup):
    cups = []
    current_cup = starting_cup
    while True:
        cups.append(current_cup)
        current_cup = cup_list[current_cup]
        if current_cup == starting_cup:
            break
    return cups


# star 1
min_cup = min(starting_cups)
max_cup = max(starting_cups)
cup_list = cyclic_cup_list(starting_cups)
simulate(cup_list, min_cup, max_cup, starting_cups[0], 100)

print("".join(str(i) for i in cup_walk(cup_list, 1)[1:]))

# star 2
ONE_MILLION = 1000000
min_cup = min(starting_cups)
max_cup = ONE_MILLION
cup_list = cyclic_cup_list_fill(starting_cups, ONE_MILLION)
simulate(cup_list, min_cup, max_cup, starting_cups[0], 10 * ONE_MILLION)

a = cup_list[1]
b = cup_list[a]
print(a * b)
