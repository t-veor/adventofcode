#!/usr/bin/env python3
import sys
from collections import deque

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


def group_by(iter, sep):
    current_group = []
    for i in iter:
        if i == sep:
            yield current_group
            current_group = []
        else:
            current_group.append(i)
    yield current_group


data = group_by(open(filename).read().splitlines(), "")
decks = [[int(j) for j in i[1:]] for i in data]


def combat(deck_one, deck_two):
    deck_one = deque(deck_one)
    deck_two = deque(deck_two)

    while deck_one and deck_two:
        card_one, card_two = deck_one.popleft(), deck_two.popleft()
        if card_one > card_two:
            deck_one.append(card_one)
            deck_one.append(card_two)
        else:
            deck_two.append(card_two)
            deck_two.append(card_one)

    return deck_one, deck_two


def score(deck):
    deck.reverse()
    score = 0
    for i, j in enumerate(deck):
        score += (i + 1) * j
    return score


def recursive_combat(deck_one, deck_two):
    seen_states = set()

    deck_one = deque(deck_one)
    deck_two = deque(deck_two)

    while deck_one and deck_two:

        state = (tuple(deck_one), tuple(deck_two))
        if state in seen_states:
            # because of the way we check for player one wins this will work,
            # since deck_one is guaranteed to have at least one card at this
            # point as otherwise it couldn't be a repeat of a previous round
            return deck_one, deck_two
        seen_states.add(state)

        card_one, card_two = deck_one.popleft(), deck_two.popleft()

        if card_one <= len(deck_one) and card_two <= len(deck_two):
            player_one_wins, _ = recursive_combat(
                list(deck_one)[:card_one], list(deck_two)[:card_two])
        else:
            player_one_wins = card_one > card_two

        if player_one_wins:
            deck_one.append(card_one)
            deck_one.append(card_two)
        else:
            deck_two.append(card_two)
            deck_two.append(card_one)

    return deck_one, deck_two



# star 1
deck_one, deck_two = combat(*decks)
winning_deck = deck_one or deck_two
print(score(winning_deck))

# star 2
deck_one, deck_two = recursive_combat(*decks)
winning_deck = deck_one or deck_two
print(score(winning_deck))
