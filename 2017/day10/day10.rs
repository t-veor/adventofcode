#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::fmt::LowerHex;

fn parse_input(input: &str) -> &str {
    input.trim()
}

#[derive(Debug)]
struct KnotHash {
    elements: [u8; 256],
    current_position: usize,
    skip_size: usize,
}

impl KnotHash {
    fn new() -> Self {
        Self {
            elements: (0..=255).collect::<Vec<_>>().try_into().unwrap(),
            current_position: 0,
            skip_size: 0,
        }
    }

    fn perform_reverse(&mut self, length: usize) {
        for i in 0..length / 2 {
            let x = (self.current_position + i) % self.elements.len();
            let y = (self.current_position + length - i - 1) % self.elements.len();

            let tmp = self.elements[x];
            self.elements[x] = self.elements[y];
            self.elements[y] = tmp;
        }

        self.current_position += length + self.skip_size;
        self.current_position %= self.elements.len();
        self.skip_size += 1;
    }

    fn round(&mut self, lengths: &[u8]) {
        for &length in lengths {
            self.perform_reverse(length as usize)
        }
    }

    fn run_rounds(&mut self, input: &[u8]) {
        let mut lengths = Vec::with_capacity(input.len() + 5);
        lengths.extend_from_slice(input);
        lengths.extend_from_slice(&[17, 31, 73, 47, 23]);

        for _ in 0..64 {
            self.round(&lengths);
        }
    }

    fn dense_hash(&self) -> Vec<u8> {
        self.elements
            .chunks(16)
            .map(|chunk| chunk.iter().fold(0, |a, &b| a ^ b))
            .map(|x| x as u8)
            .collect()
    }
}

impl LowerHex for KnotHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in self.dense_hash() {
            f.write_fmt(format_args!("{i:02x}"))?;
        }
        Ok(())
    }
}

fn star1(input: &str) -> usize {
    let lengths = input.split(',').map(|m| m.parse().unwrap());

    let mut hash = KnotHash::new();
    for length in lengths {
        hash.perform_reverse(length);
    }
    hash.elements[0] as usize * hash.elements[1] as usize
}

fn star2(input: &str) -> String {
    let mut hash = KnotHash::new();
    hash.run_rounds(input.as_bytes());
    format!("{hash:x}")
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(input));
    println!("{}", star2(input));
}
