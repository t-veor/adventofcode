def parse(input: str):
    *tiles, problems = input.split("\n\n")

    problems = problems.splitlines()

    parsed_tiles = []
    for tile in tiles:
        tile = tile.splitlines()[1:]
        count = "".join(tile).count("#")

        parsed_tiles.append((tile, count))

    parsed_problems = []
    for problem in problems:
        size, counts = problem.split(": ")
        size = tuple(int(i) for i in size.split("x"))
        counts = tuple(int(i) for i in counts.split(" "))
        parsed_problems.append((size, counts))

    return parsed_tiles, parsed_problems


with open("input.txt") as f:
    tiles, problems = parse(f.read())


definitely_possible = 0
definitely_impossible = 0
bad_cases = []

for i, (size, counts) in enumerate(problems):
    size = size[0] * size[1]

    simple_fit_bound = sum(counts) * 9
    minimum_tiles_required = sum(c * tiles[i][1] for i, c in enumerate(counts))

    assert simple_fit_bound > minimum_tiles_required

    if size >= simple_fit_bound:
        print(f"{i} POSSIBLE", size, counts, simple_fit_bound, minimum_tiles_required)
        definitely_possible += 1
    elif size < minimum_tiles_required:
        print(f"{i} IMPOSSILE", size, counts)
        definitely_impossible += 1
    else:
        bad_cases.append((i, size, counts))

print("Definitely possible:", definitely_possible)
print("Definitely impossible:", definitely_impossible)
print("Bad cases:", len(bad_cases))
