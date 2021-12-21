#!/usr/bin/env python3
import sys
import itertools
from collections import deque
import heapq

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

p1_pos, p2_pos = [int(i.split()[-1]) for i in open(filename).read().strip().splitlines()]


def simulate_deterministic(p1_pos, p2_pos):
    die = itertools.cycle(range(1, 101))
    die_roll_count = 0
    p1_score, p2_score = 0, 0

    while True:
        roll1 = sum((next(die), next(die), next(die)))
        die_roll_count += 3
        p1_pos = (p1_pos + roll1 - 1) % 10 + 1
        p1_score += p1_pos
        if p1_score >= 1000:
            break

        roll2 = sum((next(die), next(die), next(die)))
        die_roll_count += 3
        p2_pos = (p2_pos + roll2 - 1) % 10 + 1
        p2_score += p2_pos
        if p2_score >= 1000:
            break

    losing_score = min(p1_score, p2_score)
    return losing_score * die_roll_count


# possible outcomes from rolling a 3-sided die 3 times
DIRAC_OUTCOMES = [
    (3, 1),
    (4, 3),
    (5, 6),
    (6, 7),
    (7, 6),
    (8, 3),
    (9, 1),
]

def next_states(state):
    turn, p1_pos, p2_pos, p1_score, p2_score = state
    if not turn:
        for delta, mult in DIRAC_OUTCOMES:
            new_pos = (p1_pos + delta - 1) % 10 + 1
            new_state = (True, new_pos, p2_pos, p1_score + new_pos, p2_score)
            yield new_state, mult
    else:
        for delta, mult in DIRAC_OUTCOMES:
            new_pos = (p2_pos + delta - 1) % 10 + 1
            new_state = (False, p1_pos, new_pos, p1_score, p2_score + new_pos)
            yield new_state, mult


def is_end_state(state):
    _, _, _, p1_score, p2_score = state
    return p1_score >= 21 or p2_score >= 21


def simulate_dirac(p1_pos, p2_pos):
    starting_state = (False, p1_pos, p2_pos, 0, 0)

    # first of all, just generate all the nodes and how many incoming edges is
    # expected for each node
    incoming_edges = {
        starting_state: 0
    }
    explored = set()
    queue = deque()
    queue.append(starting_state)

    while queue:
        state = queue.popleft()
        if state in explored:
            continue
        explored.add(state)

        if is_end_state(state):
            continue

        for next_state, _ in next_states(state):
            incoming_edges[next_state] = incoming_edges.get(next_state, 0) + 1
            queue.append(next_state)

    # now that we have a count of incoming edges for each state we can explore
    # them in topological order

    heap = [(j, i) for i, j in incoming_edges.items()]
    heapq.heapify(heap)
    seen = set()

    def choose_state():
        while heap:
            _, state = heapq.heappop(heap)
            if state in seen:
                continue
            seen.add(state)

            if incoming_edges[state] == 0:
                return state
            else:
                return None
        return None

    def decrease_key(state):
        incoming_edges[state] -= 1
        heapq.heappush(heap, (incoming_edges[state], state))

    states = {
        starting_state: 1
    }

    while True:
        state = choose_state()
        if state is None:
            break

        if is_end_state(state):
            continue

        count = states[state]

        for next_state, mult in next_states(state):
            decrease_key(next_state)
            states[next_state] = states.get(next_state, 0) + mult * count

    assert all(i == 0 for i in incoming_edges.values())

    p1_wins = 0
    p2_wins = 0
    for (_, _, _, p1_score, p2_score), count in states.items():
        if p1_score >= 21:
            p1_wins += count
        elif p2_score >= 21:
            p2_wins += count

    return (p1_wins, p2_wins)


# star 1
print(simulate_deterministic(p1_pos, p2_pos))

# star 2
print(max(simulate_dirac(p1_pos, p2_pos)))
