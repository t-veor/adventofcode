#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn priority(byte: u8) -> u8 {
    if (b'a'..=b'z').contains(&byte) {
        byte - b'a' + 1
    } else {
        byte - b'A' + 27
    }
}

// Let's do something a bit more fun for day 3 -- implementing our own bitset
#[derive(Debug, Clone, Copy)]
struct BitSet(u64);

impl BitSet {
    const ALL: Self = BitSet(u64::MAX);

    fn new(s: impl IntoIterator<Item = u8>) -> Self {
        let mut bits = 0;
        for byte in s {
            bits |= 1 << byte;
        }
        Self(bits)
    }

    fn intersect(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }
}

impl IntoIterator for BitSet {
    type Item = u8;
    type IntoIter = BitSetIter;

    fn into_iter(self) -> Self::IntoIter {
        BitSetIter {
            remaining: self.0,
            curr_offset: 0,
        }
    }
}

struct BitSetIter {
    remaining: u64,
    curr_offset: u32,
}

impl Iterator for BitSetIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            let next_offset = self.remaining.trailing_zeros();
            self.curr_offset += next_offset + 1;
            self.remaining >>= next_offset + 1;
            Some((self.curr_offset - 1) as u8)
        }
    }
}

fn parse_input(input: &str) -> Vec<&str> {
    input.lines().collect()
}

fn star1(input: &[&str]) -> i32 {
    input
        .iter()
        .flat_map(|line| {
            let line = line.as_bytes();
            let (first, second) = line.split_at(line.len() / 2);

            let first = BitSet::new(first.iter().copied().map(priority));
            let second = BitSet::new(second.iter().copied().map(priority));

            (first.intersect(second)).into_iter().map(|x| x as i32)
        })
        .sum()
}

fn star2(input: &[&str]) -> i32 {
    input
        .chunks_exact(3)
        .flat_map(|chunk| {
            chunk
                .iter()
                .map(|line| BitSet::new(line.as_bytes().iter().copied().map(priority)))
                .fold(BitSet::ALL, BitSet::intersect)
                .into_iter()
                .map(|x| x as i32)
        })
        .sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
