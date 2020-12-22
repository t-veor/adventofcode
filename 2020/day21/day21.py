#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

foods = []
for line in open(filename).read().splitlines():
    ingredients, allergens = line.split(" (contains ")
    ingredients = ingredients.split(" ")
    allergens = allergens[:-1].split(", ")
    foods.append((ingredients, allergens))

allergen_sets = {}
for ingredients, allergens in foods:
    for allergen in allergens:
        if allergen not in allergen_sets:
            allergen_sets[allergen] = set(ingredients)
        else:
            allergen_sets[allergen] &= set(ingredients)

allergen_map = {}
while allergen_sets:
    for allergen, possible in allergen_sets.items():
        if len(possible) == 1:
            ingredient = next(iter(possible))
            allergen_map[ingredient] = allergen
            del allergen_sets[allergen]
            for i in allergen_sets.values():
                if ingredient in i:
                    i.remove(ingredient)
            break
        elif len(possible) == 0:
            raise Exception("found allergen with no candidate ingredient '{}'".format(allergen))
    else:
        raise Exception("could not make progress, all remaining allergens have more than one candidate")

# star 1
count = 0
for ingredients, _ in foods:
    for ingredient in ingredients:
        if ingredient not in allergen_map:
            count += 1
print(count)

# star 2
print(",".join(i[1] for i in sorted((k, j) for j, k in allergen_map.items())))
