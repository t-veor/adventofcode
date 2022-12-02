#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

strategy_guide = [tuple(i.split(" ")) for i in open(filename).read().splitlines()]

# star 1
def score_game_1(opponent_move, your_move):
    opponent_move = ord(opponent_move) - ord("A")
    your_move = ord(your_move) - ord("X")
    # maps a win to 2, draw to 1, and loss to 0
    outcome = (your_move - opponent_move + 1) % 3

    return your_move + 1 + outcome * 3

print(sum(score_game_1(i, j) for i, j in strategy_guide))

# star 2
def score_game_2(opponent_move, outcome):
    opponent_move = ord(opponent_move) - ord("A")
    outcome = ord(outcome) - ord("X")

    # since we know that your_move - opponent_move + 1 `equiv` outcome (mod 3)
    # it follows that your_move `equiv` outcome + opponent_move - 1 (mod 3)
    your_move = (outcome + opponent_move - 1) % 3

    return your_move + 1 + outcome * 3

print(sum(score_game_2(i, j) for i, j in strategy_guide))