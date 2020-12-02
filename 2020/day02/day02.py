#!/usr/bin/env python3
import sys
import re
from collections import namedtuple

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

Password = namedtuple("Password", ["min", "max", "req", "password"])

passwords = []
for i in open(filename).read().splitlines():
    match = re.match(r"^(\d+)-(\d+) (.): (.+)$", i)
    min_count, max_count = int(match.group(1)), int(match.group(2))
    passwords.append(
        Password(min_count, max_count, match.group(3), match.group(4)))


def is_valid_star1(password):
    count = password.password.count(password.req)
    return password.min <= count <= password.max


def is_valid_star2(password):
    first = password.password[password.min - 1] == password.req
    second = password.password[password.max - 1] == password.req
    return first ^ second


# star 1
count = 0
for password in passwords:
    if is_valid_star1(password):
        count += 1
print(count)

# star 2
count = 0
for password in passwords:
    if is_valid_star2(password):
        count += 1
print(count)
