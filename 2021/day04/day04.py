#!/usr/bin/env python3
import sys


class Board:
    def __init__(self, lines):
        self.numbers = []
        self.matched = []

        for l in lines:
            ns = [int(i) for i in l.split()]
            self.numbers.append(ns)
            self.matched.append([False] * len(ns))

    def mark(self, number):
        for i, row in enumerate(self.numbers):
            for j, n in enumerate(row):
                if n == number:
                    self.matched[i][j] = True

    def is_winning(self):
        for i, row in enumerate(self.matched):
            if all(row):
                return True

        for i in range(len(self.matched[0])):
            if all(row[i] for row in self.matched):
                return True

        return False

    def score(self):
        score = 0
        for i, row in enumerate(self.matched):
            for j, matched in enumerate(row):
                if not matched:
                    score += self.numbers[i][j]
        return score


if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().splitlines()

order = [int(i) for i in data[0].split(",")]

# star 1
boards = []
for i in range(2, len(data), 6):
    boards.append(Board(data[i:i+5]))

won = False
for n in order:
    for board in boards:
        board.mark(n)
        if board.is_winning():
            print(board.score() * n)
            won = True
            break
    if won:
        break

# star 2
boards = []
for i in range(2, len(data), 6):
    boards.append(Board(data[i:i+5]))
last_board = None
for n in order:
    for board in boards:
        board.mark(n)
        if board.is_winning():
            last_board = board
    boards = [i for i in boards if not i.is_winning()]
    if not boards:
        break
print(last_board.score() * n)
