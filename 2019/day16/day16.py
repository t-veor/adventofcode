#!/usr/bin/env python3
import sys
import itertools

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

data = [int(i) for i in open(filename).read().strip()]

def fft(signal, rounds):
    for _ in range(rounds):
        result = []
        for i in range(len(signal)):
            val = 0

            try:
                for j in range(i, len(signal), 4 * (i + 1)):
                    for k in range(i + 1):
                        val += signal[j + k]
                    for k in range(i + 1):
                        val -= signal[2 * (i + 1) + j + k]
            except IndexError:
                pass

            val = abs(val) % 10
            result.append(val)
        signal = result

    return signal


def fft_second_half_of_input(signal, reps, offset, rounds):
    # when offset > len(signal) * reps / 2,
    # result[offset] is just sum(signal[offset:])
    signal = list(signal[i % len(signal)] for i in range(offset, len(signal) * reps))

    for _ in range(rounds):
        result = [0] * len(signal)
        first = True
        for i in range(len(signal) - 1, -1, -1):
            if first:
                first = False
                result[i] = signal[i]
            else:
                result[i] = signal[i] + result[i + 1]

        for i in range(len(result)):
            result[i] = abs(result[i]) % 10

        signal = result

    return signal


# star 1
print("".join(str(i) for i in fft(data, 100)[:8]))

# star 2
offset = int("".join(str(i) for i in data[:7]))
assert(offset > len(data) * 10000 / 2)
print("".join(str(i) for i in fft_second_half_of_input(data, 10000, offset, 100)[:8]))
