#!/usr/bin/env python3
import sys

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

fields, my_ticket, nearby_tickets = group_by(open(filename).read().splitlines(), "")
for i, j in enumerate(fields):
    field_name, ranges = j.split(": ")
    ranges = ranges.split(" or ")
    ranges = tuple(tuple(int(j) for j in i.split("-")) for i in ranges)
    fields[i] = (field_name, ranges)
my_ticket = [int(i) for i in my_ticket[1].split(",")]
nearby_tickets = [[int(j) for j in i.split(",")] for i in nearby_tickets[1:]]


def check_in_ranges(ranges, value):
    for upper, lower in ranges:
        if upper <= value <= lower:
            return True
    return False


# star 1
valid_tickets = []
error_rate = 0
for ticket in nearby_tickets:
    valid = True
    for value in ticket:
        if all(not check_in_ranges(ranges, value) for _, ranges in fields):
            error_rate += value
            valid = False
    if valid:
        valid_tickets.append(ticket)
print(error_rate)

# star 2
valid_tickets.append(my_ticket)
possible_field_assignments = {}
for field, ranges in fields:
    possible_assignments = []
    for i in range(len(my_ticket)):
        if all(check_in_ranges(ranges, ticket[i]) for ticket in valid_tickets):
            possible_assignments.append(i)
    possible_field_assignments[field] = possible_assignments
# we could use a real bipartite graph matching algorithm, but that's way too
# much effort
field_assignments = {}
while possible_field_assignments:
    for field, possible_assignments in possible_field_assignments.items():
        if len(possible_assignments) == 1:
            field_assignments[field] = possible_assignments[0]
            del possible_field_assignments[field]
            for i in possible_field_assignments.values():
                i.remove(possible_assignments[0])
            break
        elif len(possible_assignments) == 0:
            raise "Impossible configuration - {} can't be deduced".format(field)
    else:
        raise "No progress being made - assignments are ambiguous?"

product = 1
for field, index in field_assignments.items():
    if field.startswith("departure"):
        product *= my_ticket[index]
print(product)


