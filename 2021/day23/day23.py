#!/usr/bin/env python3
import sys
import heapq

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


class AmphipodMap:
    def __init__(self, corridor=None, rooms=None):
        if corridor is None:
            self.corridor = [None] * 7
        else:
            self.corridor = corridor
        if rooms is None:
            self.rooms = [[None, None] for _ in range(4)]
        else:
            self.rooms = rooms

    def augment(self):
        augments = [
            ["D", "D"],
            ["C", "B"],
            ["B", "A"],
            ["A", "C"],
        ]
        rooms = []
        for room, augment in zip(self.rooms, augments):
            rooms.append([room[0]] + augment + [room[-1]])

        return AmphipodMap(None, rooms)

    def clone(self):
        corridor = self.corridor[:]
        rooms = [room[:] for room in self.rooms]
        return AmphipodMap(corridor, rooms)

    def done(self):
        for i, room in enumerate(self.rooms):
            expected = chr(i + ord("A"))
            if any(j != expected for j in room):
                return False
        return True

    def __eq__(self, other):
        return self.corridor == other.corridor and self.rooms == other.rooms

    def __hash__(self):
        return hash(tuple(self.corridor + [j for i in self.rooms for j in i]))

    def __str__(self):
        rows = []
        rows.append("#" * 13)
        corridor = [i or "." for i in self.corridor]
        rows.append("#" + corridor[0] + ".".join(corridor[1:6]) + corridor[6] + "#")
        for i in range(len(self.rooms[0])):
            header = "###" if i == 0 else "  #"
            footer = "###" if i == 0 else "#"
            rows.append(header + "#".join(room[i] or "." for room in self.rooms) + footer)
        rows.append("  " + "#" * 9)
        return "\n".join(rows)

    @staticmethod
    def target_room(amphi):
        return ord(amphi) - ord("A")

    @staticmethod
    def multiplier(amphi):
        if amphi == "A":
            return 1
        elif amphi == "B":
            return 10
        elif amphi == "C":
            return 100
        else:
            return 1000

    def heuristic(self):
        min_cost_required = 0
        for corridor_pos, amphi in enumerate(self.corridor):
            if amphi is None:
                continue
            target_room = self.target_room(amphi)
            multiplier = self.multiplier(amphi)

            moves_required = self.room_to_corridor_move_count(target_room, 0, corridor_pos)
            min_cost_required += moves_required * multiplier

        for room_number, room in enumerate(self.rooms):
            for room_pos, amphi in enumerate(room):
                if amphi is None:
                    continue
                target_room = self.target_room(amphi)
                if target_room == room_number:
                    continue

                multiplier = self.multiplier(amphi)

                moves_required = self.room_to_room_move_count(room_number, room_pos, target_room)
                min_cost_required += moves_required * multiplier

        return min_cost_required

    def possible_moves(self):
        yield from self.possible_room_moves()
        yield from self.possible_corridor_moves()

    @staticmethod
    def room_to_corridor_move_count(room_number, room_pos, corridor_pos):
        move_count = room_pos + 1
        room_corridor_pos = room_number + 1.5
        move_count += int(2 * abs(corridor_pos - room_corridor_pos))
        if corridor_pos == 0 or corridor_pos == 6:
            move_count -= 1
        return move_count

    @staticmethod
    def room_to_room_move_count(room_number, room_pos, target_room):
        move_count = room_pos
        room_diff = abs(room_number - target_room)
        move_count += 2 * room_diff + 2
        return move_count

    def unchecked_move(self, room_number, room_pos, corridor_pos):
        cloned = self.clone()
        a = self.rooms[room_number][room_pos]
        b = self.corridor[corridor_pos]
        cloned.rooms[room_number][room_pos] = b
        cloned.corridor[corridor_pos] = a

        move_count = self.room_to_corridor_move_count(room_number, room_pos, corridor_pos)
        return move_count, cloned

    def possible_room_moves(self):
        for room_number, room in enumerate(self.rooms):
            for amphi_pos, amphipod in enumerate(room):
                if amphipod is not None:
                    break
            else:
                # no amphipods in this room
                continue

            starting_corridor_pos = room_number + 1
            # scan for accessible part of the corridor
            min_corridor = 0
            for i in range(starting_corridor_pos, -1, -1):
                if self.corridor[i] is not None:
                    min_corridor = i + 1
                    break
            max_corridor = 6
            for i in range(starting_corridor_pos + 1, 7):
                if self.corridor[i] is not None:
                    max_corridor = i - 1
                    break

            multiplier = self.multiplier(amphipod)
            # possible moves to corridors
            for i in range(min_corridor, max_corridor + 1):
                move_count, new_map = self.unchecked_move(room_number, amphi_pos, i)
                yield move_count * multiplier, new_map

    def possible_corridor_moves(self):
        for corridor_pos, amphi in enumerate(self.corridor):
            if amphi is None:
                continue
            target_room = self.target_room(amphi)
            if any(i != amphi and i is not None for i in self.rooms[target_room]):
                continue

            for room_pos in range(len(self.rooms[target_room]) - 1, -1, -1):
                if self.rooms[target_room][room_pos] is None:
                    break

            room_corridor_pos = target_room + 1
            impassible = False
            if corridor_pos <= room_corridor_pos:
                for i in range(corridor_pos + 1, room_corridor_pos + 1):
                    if self.corridor[i] is not None:
                        impassible = True
                        break
            else:
                for i in range(room_corridor_pos + 1, corridor_pos):
                    if self.corridor[i] is not None:
                        impassible = True
                        break
            if impassible:
                continue

            multiplier = self.multiplier(amphi)
            move_count, new_map = self.unchecked_move(target_room, room_pos, corridor_pos)
            yield move_count * multiplier, new_map


class AmphipodMapEntry:
    def __init__(self, cost, map):
        self.heuristic = cost + map.heuristic()
        self.cost = cost
        self.map = map

    def explode(self):
        return self.cost, self.map

    def __lt__(self, other):
        return self.heuristic < other.heuristic


grid = open(filename).read().splitlines()
rooms = []
for i in range(4):
    rooms.append([grid[2][3 + 2 * i], grid[3][3 + 2 * i]])

map = AmphipodMap(None, rooms)


def a_star(start):
    queue = [AmphipodMapEntry(0, start)]
    comes_from = {}
    heapq.heapify(queue)
    explored = set()

    while queue:
        cost, node = heapq.heappop(queue).explode()
        if node in explored:
            continue
        explored.add(node)

        if node.done():
            # We're done!
            reconstructed_path = []
            reconstructed_path.append((cost, node))
            curr_cost, curr_node = cost, node
            while curr_node in comes_from:
                curr_cost, curr_node = comes_from[curr_node]
                reconstructed_path.append((curr_cost, curr_node))
            return list(reversed(reconstructed_path)), cost

        for weight, new_node in node.possible_moves():
            new_cost = cost + weight
            if new_node in comes_from:
                old_cost, _ = comes_from[new_node]
                if new_cost >= old_cost:
                    continue
            comes_from[new_node] = (new_cost, node)
            heapq.heappush(queue, AmphipodMapEntry(new_cost, new_node))


# star 1
_, cost = a_star(map)
print(cost)

# star 2
augmented_map = map.augment()
_, cost = a_star(augmented_map)
print(cost)
