#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


TILE_SIZE = 10


def group_by(iter, sep):
    current_group = []
    for i in iter:
        if i == sep:
            yield current_group
            current_group = []
        else:
            current_group.append(i)
    yield current_group


raw_tiles = [i for i in group_by(open(filename).read().splitlines(), "") if i]


def parse_tile(raw_tile):
    id = raw_tile[0][len("Tile "):-1]
    raw_tile = raw_tile[1:]

    edges = []
    edges.append(raw_tile[0])
    edges.append("".join(raw_tile[i][-1] for i in range(len(raw_tile))))
    edges.append(raw_tile[-1][::-1])
    edges.append("".join(raw_tile[i][0] for i in range(len(raw_tile)))[::-1])

    data = [i[1:-1] for i in raw_tile[1:-1]]

    return id, edges, data


tile_edges = {}
tiles_by_edge = {}
tile_data = {}

for raw_tile in raw_tiles:
    id, edges, data = parse_tile(raw_tile)
    tile_edges[id] = edges
    tile_data[id] = data

    for edge in edges:
        if edge not in tiles_by_edge:
            tiles_by_edge[edge] = []
        tiles_by_edge[edge].append(id)
        edge = edge[::-1]
        if edge not in tiles_by_edge:
            tiles_by_edge[edge] = []
        tiles_by_edge[edge].append(id)



"""
Orientation representation:
The edges of each tile are indexed [0, 1, 2, 3] in clockwise order.
Orientations 0-3 refer to unflipped tiles oriented north, east, etc.
i.e.
       ^
       |
   +-------+            +-------+            +-------+             +-------+
   |   a   |            |   d   |            |   c   |             |   b   |
   | d   b |            | c   a |-->         | b   d |          <--| a   c |
   |   c   |            |   b   |            |   a   |             |   d   |
   +-------+            +-------+            +-------+             +-------+
                                                 |
                                                 v
   Orientation 0        Orientation 1        Orientation 2         Orientation 3
   Edges [a, b, c, d]   Edges [d, a, b, c]   Edges [c, d, a, b]    Edges [b, c, d, a]

Flipped orientations are represented using 4-7. They are reflected across the 45-degree
diagonal in this direction: \\
i.e.
                                                                       ^
                                                                       |
   +-------+            +-------+            +-------+             +-------+
   |   d   |            |   c   |            |   b   |             |   a   |
<--| a   c |            | d   b |            | c   a |-->          | b   d |
   |   b   |            |   a   |            |   d   |             |   c   |
   +-------+            +-------+            +-------+             +-------+
                            |
                            v
   Orientation 4        Orientation 5        Orientation 6         Orientation 7
   Edges [d, c, b, a]   Edges [c, b, a, d]   Edges [b, a, d, c]    Edges [a, d, c, b]
"""


def rotate_edges(edges, orientation):
    rot = orientation % 4
    edges = edges[-rot:] + edges[:-rot]
    if orientation >= 4:
        edges = [i[::-1] for i in reversed(edges)]
    return edges


def rotate_tile_data(data, orientation):
    assert(len(data) == len(data[0]))
    rot = orientation % 4
    flip = orientation >= 4

    new_data = []
    n = len(data)
    for y in range(n):
        row = ""
        for x in range(n):
            old_x, old_y = x, y
            if flip:
                old_x, old_y = y, x

            if rot == 1:
                old_x, old_y = old_y, n - 1 - old_x
            elif rot == 2:
                old_x, old_y = n - 1 - old_x, n - 1 - old_y
            elif rot == 3:
                old_x, old_y = n - 1 - old_y, old_x
            row += data[old_y][old_x]
        new_data.append(row)
    return new_data


class TileState:
    def __init__(self, constraints, positions_by_tile, tiles_by_position):
        self.constraints = constraints
        self.positions_by_tile = positions_by_tile
        self.tiles_by_position = tiles_by_position

    def copy(self):
        return TileState(
            dict((i, dict(j)) for i, j in self.constraints.items()),
            self.positions_by_tile.copy(),
            self.tiles_by_position.copy())

    def place_tile(self, tile_id, position, orientation, rotated_edges):
        self.tiles_by_position[position] = (tile_id, orientation)
        self.positions_by_tile[tile_id] = position
        if position in self.constraints:
            del self.constraints[position]

        x, y = position
        new_positions = [(x, y-1), (x+1, y), (x, y+1), (x-1, y)]
        constraint_dirs = [2, 3, 0, 1]
        for new_position, dir, edge in zip(new_positions, constraint_dirs, rotated_edges):
            if new_position not in self.tiles_by_position:
                if new_position not in self.constraints:
                    self.constraints[new_position] = {}

                self.constraints[new_position][dir] = edge[::-1]

    def extent(self):
        min_x = min(i[0] for i in self.tiles_by_position.keys())
        max_x = max(i[0] for i in self.tiles_by_position.keys())
        min_y = min(i[1] for i in self.tiles_by_position.keys())
        max_y = max(i[1] for i in self.tiles_by_position.keys())

        return (min_x, max_x), (min_y, max_y)

    def print(self, tile_edges):
        (min_x, max_x), (min_y, max_y) = self.extent()

        for y in range(min_y, max_y + 1):
            for row in range(TILE_SIZE):
                for x in range(min_x, max_x + 1):
                    if (x, y) in self.tiles_by_position:
                        tile_id, orientation = self.tiles_by_position[x, y]
                        edges = rotate_edges(tile_edges[tile_id], orientation)
                        if row == 0:
                            print(edges[0], end="")
                        elif row == TILE_SIZE - 1:
                            print(edges[2][::-1], end="")
                        else:
                            print(edges[3][TILE_SIZE - row - 1], end="")
                            if row == TILE_SIZE // 2:
                                name = "({},{})".format(tile_id, orientation)
                                left = (TILE_SIZE - len(name) - 2) // 2
                                right = TILE_SIZE - len(name) - 2 - left
                                print(" " * left, end="")
                                print(name, end="")
                                print(" " * right, end="")
                            else:
                                print(" " * (TILE_SIZE - 2), end="")
                            print(edges[1][row], end="")
                    else:
                        print(" " * TILE_SIZE, end="")
                print()

    def assemble_image(self, tile_data):
        (min_x, max_x), (min_y, max_y) = self.extent()
        STRIPPED_TILE_SIZE = TILE_SIZE - 2

        image = []
        for y in range(min_y, max_y + 1):
            tiles = []
            for x in range(min_x, max_x + 1):
                tile_id, orientation = self.tiles_by_position[x, y]
                tiles.append(rotate_tile_data(tile_data[tile_id], orientation))
            for row in range(STRIPPED_TILE_SIZE):
                image.append("".join(i[row] for i in tiles))
        return image





def solve(tile_edges, tiles_by_edge):
    def place_tile(tile_state, tile_id, position, orientation):
        edges = rotate_edges(tile_edges[tile_id], orientation)

        tile_state = tile_state.copy()
        tile_state.place_tile(tile_id, position, orientation, edges)

        return tile_state

    def tiles_that_match_constraints(constraints):
        tile_set = [set(tiles_by_edge.get(i, [])) for i in constraints.values()]
        matching_tiles = set.intersection(*tile_set)

        for tile_id in matching_tiles:
            edges = tile_edges[tile_id]
            for orientation in range(8):
                rotated_edges = rotate_edges(edges, orientation)
                for dir, constraint in constraints.items():
                    if rotated_edges[dir] != constraint:
                        break
                else:
                    yield orientation, tile_id

    def backtrack(tile_state):
        # print(tile_state.constraints)
        # tile_state.print(tile_edges)
        if len(tile_state.positions_by_tile) == len(tile_edges):
            return tile_state

        for position, constraint in tile_state.constraints.items():
            for orientation, tile_id in tiles_that_match_constraints(constraint):
                # check if the tile has already been placed
                if tile_id in tile_state.positions_by_tile:
                    continue

                # print("Trying to place tile {} at {}, {}".format(tile_id, position, orientation))
                new_tile_state = place_tile(tile_state, tile_id, position, orientation)
                result = backtrack(new_tile_state)

                if result is not None:
                    return result

        # print("backtrack")
        return None

    tile_state = TileState({}, {}, {})
    random_tile, edges = list(tile_edges.items())[0]
    tile_state.place_tile(random_tile, (0, 0), 0, edges)

    return backtrack(tile_state)


DRAGON = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
]
DRAGON_WIDTH = len(DRAGON[0])
DRAGON_HEIGHT = len(DRAGON)
DRAGON_PIXELS = []
for y in range(DRAGON_HEIGHT):
    for x in range(DRAGON_WIDTH):
        if DRAGON[y][x] == "#":
            DRAGON_PIXELS.append((x, y))


def find_dragons(image):
    image_width = len(image[0])
    image_height = len(image)

    dragon_count = 0
    for y in range(image_height - DRAGON_HEIGHT):
        for x in range(image_width - DRAGON_WIDTH):
            for dx, dy in DRAGON_PIXELS:
                if image[y + dy][x + dx] != "#":
                    break
            else:
                dragon_count += 1

    return dragon_count



# star 1
tile_state = solve(tile_edges, tiles_by_edge)
(min_x, max_x), (min_y, max_y) = tile_state.extent()
a, _ = tile_state.tiles_by_position[min_x, min_y]
b, _ = tile_state.tiles_by_position[min_x, max_y]
c, _ = tile_state.tiles_by_position[max_x, min_y]
d, _ = tile_state.tiles_by_position[max_x, max_y]
print(int(a) * int(b) * int(c) * int(d))

# star 2
image = tile_state.assemble_image(tile_data)
for orientation in range(8):
    rotated_image = rotate_tile_data(image, orientation)
    dragon_count = find_dragons(rotated_image)
    if dragon_count > 0:
        # assume that all dragons are nonoverlapping
        print("".join(image).count("#") - dragon_count * len(DRAGON_PIXELS))
        break
