#!/usr/bin/env python3
import sys
import re

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


passports = []
groups = [i for i in group_by(open(filename).read().splitlines(), "") if i]
for group in groups:
    passport = {}
    for line in group:
        fields = line.split(" ")
        for field in fields:
            name, data = field.split(":")
            passport[name] = data
    passports.append(passport)


def year_validator(start, end, length):
    def validate(year: str):
        if len(year) != length or not year.isdigit():
            return False
        return start <= int(year) <= end
    return validate


def validate_height(height):
    match = re.match(r"(\d+)(cm|in)$", height)
    if not match:
        return False
    num, unit = int(match.group(1)), match.group(2)
    if unit == "cm":
        return 150 <= num <= 193
    else:
        return 59 <= num <= 76


def validate_color(color: str):
    hex_digits = "0123456789abcdef"
    return len(color) == 7 and color[0] == "#" and all(c in hex_digits for c in color[1:])


# star 1
required_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
count = 0
for passport in passports:
    if all(i in passport for i in required_fields):
        count += 1
print(count)

# star 2
validators = {
    "byr": year_validator(1920, 2002, 4),
    "iyr": year_validator(2010, 2020, 4),
    "eyr": year_validator(2020, 2030, 4),
    "hgt": validate_height,
    "hcl": validate_color,
    "ecl": lambda i: i in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
    "pid": lambda i: len(i) == 9 and i.isdigit(),
}
count = 0
for passport in passports:
    valid = True
    for field, validator in validators.items():
        if field not in passport or not validator(passport[field]):
            valid = False
            break
    if valid:
        count += 1
print(count)
