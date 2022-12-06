#!/usr/bin/env python3
import sys
from collections import deque

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = open(filename).read().strip()

class CharQueue:
    def __init__(self, max_size):
        self.max_size = max_size
        self.queue = deque()
        self.freqs = [0] * 26
    
    def push(self, char):
        idx = ord(char) - ord("a")
        self.queue.append(idx)
        self.freqs[idx] += 1
        while len(self.queue) > self.max_size:
            removed_idx = self.queue.popleft()
            self.freqs[removed_idx] -= 1
    
    def all_different(self):
        return len(self.queue) == self.max_size and all(i <= 1 for i in self.freqs)

# star 1
queue = CharQueue(4)
for i, c in enumerate(data):
    queue.push(c)
    if queue.all_different():
        print(i + 1)
        break
else:
    print("not found")

# star 2
queue = CharQueue(14)
for i, c in enumerate(data):
    queue.push(c)
    if queue.all_different():
        print(i + 1)
        break
else:
    print("not found")