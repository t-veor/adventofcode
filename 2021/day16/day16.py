#!/usr/bin/env python3
import sys

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"


hexstring = open(filename).read().strip()


def parse_hex_into_bitstring(hexstring):
    nibbles = []
    for i in hexstring:
        nibbles.append(f"{int(i, base=16):04b}")
    return "".join(nibbles)


bitstring = parse_hex_into_bitstring(hexstring)


class Packet:
    def __init__(self, version, type, value, children):
        self.version = version
        self.type = type
        self.value = value
        self.children = children

    def __repr__(self):
        if self.value is not None:
            coda = f"value={self.value}"
        else:
            coda = f"children={repr(self.children)}"
        return f"Packet(version={self.version}, type={self.type}, {coda})"

    @staticmethod
    def parse(bitstring):
        _, packet = Packet._parse(bitstring, 0)
        return packet

    @staticmethod
    def _parse(bitstring, pos):
        version = int(bitstring[pos:pos+3], base=2)
        pos += 3
        type = int(bitstring[pos:pos+3], base=2)
        pos += 3
        if type == 4:
            literal_bits = []
            while True:
                group = bitstring[pos:pos+5]
                pos += 5
                continue_flag = group[0] == "1"
                literal_bits.append(group[1:])
                if not continue_flag:
                    break
            value = int("".join(literal_bits), base=2)
            return pos, Packet(version, type, value, [])
        else:
            children = []
            length_type = bitstring[pos]
            pos += 1
            if length_type == "0":
                subpacket_length = int(bitstring[pos:pos+15], base=2)
                pos += 15
                expected_end = pos + subpacket_length
                while pos < expected_end:
                    pos, subpacket = Packet._parse(bitstring, pos)
                    children.append(subpacket)
            else:
                subpacket_count = int(bitstring[pos:pos+11], base=2)
                pos += 11
                for _ in range(subpacket_count):
                    pos, subpacket = Packet._parse(bitstring, pos)
                    children.append(subpacket)
            return pos, Packet(version, type, None, children)

    def sum_version_numbers(self):
        return self.version + sum(i.sum_version_numbers() for i in self.children)

    def eval(self):
        children_values = (i.eval() for i in self.children)
        if self.type == 0:
            return sum(children_values)
        elif self.type == 1:
            product = 1
            for x in children_values:
                product *= x
            return product
        elif self.type == 2:
            return min(children_values)
        elif self.type == 3:
            return max(children_values)
        elif self.type == 4:
            return self.value
        elif self.type == 5:
            val1, val2 = tuple(children_values)
            return int(val1 > val2)
        elif self.type == 6:
            val1, val2 = tuple(children_values)
            return int(val1 < val2)
        else:
            val1, val2 = tuple(children_values)
            return int(val1 == val2)


root = Packet.parse(bitstring)

# star 1
print(root.sum_version_numbers())

# star 2
print(root.eval())
