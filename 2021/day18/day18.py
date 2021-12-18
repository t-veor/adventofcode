#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


class SnailfishNumber:
    def __init__(self, value, left, right):
        assert value is not None or (left is not None and right is not None)

        self.value = value
        self.left = left
        if left is not None:
            left.parent = self
        self.right = right
        if right is not None:
            right.parent = self
        self.parent = None

    def __repr__(self):
        if self.value is not None:
            return repr(self.value)
        else:
            return f"[{repr(self.left)},{repr(self.right)}]"

    @staticmethod
    def parse(string):
        _, number = SnailfishNumber._parse(string, 0)
        return number

    @staticmethod
    def _parse(string, pos):
        if string[pos].isdigit():
            end = pos
            while end < len(string) and string[end].isdigit():
                end += 1
            value = int(string[pos:end])
            return end, SnailfishNumber(value, None, None)

        assert string[pos] == "[", f"unexpected {string[pos]}, expected ["
        pos += 1

        pos, left = SnailfishNumber._parse(string, pos)

        assert string[pos] == ",", f"unexpected {string[pos]}, expected ,"
        pos += 1

        pos, right = SnailfishNumber._parse(string, pos)

        assert string[pos] == "]", f"unexpected {string[pos]}, expected ]"
        pos += 1

        return pos, SnailfishNumber(None, left, right)

    def rightmost_leaf(self):
        if self.value is not None:
            return self
        else:
            return self.right.rightmost_leaf()

    def leftmost_leaf(self):
        if self.value is not None:
            return self
        else:
            return self.left.leftmost_leaf()

    def previous_leaf(self):
        if self.parent is None:
            return None
        elif self.parent.left is self:
            return self.parent.previous_leaf()
        else:
            return self.parent.left.rightmost_leaf()

    def next_leaf(self):
        if self.parent is None:
            return None
        elif self.parent.right is self:
            return self.parent.next_leaf()
        else:
            return self.parent.right.leftmost_leaf()

    def explode(self):
        left, right = self.left.value, self.right.value
        assert left is not None and right is not None

        prev_node = self.previous_leaf()
        next_node = self.next_leaf()

        if prev_node is not None:
            prev_node.value += left
        if next_node is not None:
            next_node.value += right

        self.left = None
        self.right = None
        self.value = 0

    def split(self):
        assert self.value is not None

        left = self.value // 2
        right = self.value - left

        self.value = None
        self.left = SnailfishNumber(left, None, None)
        self.left.parent = self
        self.right = SnailfishNumber(right, None, None)
        self.right.parent = self

    def try_explode(self, depth=0):
        if (
            self.left is not None
            and self.left.value is not None
            and self.right is not None
            and self.right.value is not None
            and depth >= 4
        ):
            self.explode()
            return True

        if self.left is not None and self.left.try_explode(depth + 1):
            return True

        if self.right is not None and self.right.try_explode(depth + 1):
            return True

        return False

    def try_split(self):
        if self.value is not None and self.value >= 10:
            self.split()
            return True

        if self.left is not None and self.left.try_split():
            return True

        if self.right is not None and self.right.try_split():
            return True

        return False

    def reduce(self):
        while True:
            if self.try_explode():
                continue
            if self.try_split():
                continue
            break

    def clone(self):
        if self.value is not None:
            return SnailfishNumber(self.value, None, None)
        else:
            return SnailfishNumber(None, self.left.clone(), self.right.clone())

    @staticmethod
    def add(a, b):
        new_number = SnailfishNumber(None, a.clone(), b.clone())
        new_number.reduce()
        return new_number

    def magnitude(self):
        if self.value is not None:
            return self.value

        return self.left.magnitude() * 3 + self.right.magnitude() * 2


numbers = [SnailfishNumber.parse(i) for i in open(filename).read().strip().splitlines()]

# star 1
acc = numbers[0]
for i in numbers[1:]:
    acc = SnailfishNumber.add(acc, i)
print(acc.magnitude())

# star 2
max_magnitude = 0
for i in range(len(numbers)):
    for j in range(len(numbers)):
        if i == j:
            continue
        a = numbers[i]
        b = numbers[j]
        result = SnailfishNumber.add(a, b)
        max_magnitude = max(max_magnitude, result.magnitude())
print(max_magnitude)
