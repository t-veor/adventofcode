#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::fmt::LowerHex;

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

    fn compute(input: &str) -> u128 {
        let mut hash = Self::new();
        hash.run_rounds(input.as_bytes());
        u128::from_be_bytes(hash.dense_hash().try_into().unwrap())
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

struct DiskGrid {
    disk: [u128; 128],
}

impl DiskGrid {
    fn new() -> Self {
        Self { disk: [0; 128] }
    }

    fn from_knot_hash(seed: &str) -> Self {
        Self {
            disk: (0..128)
                .map(|i| KnotHash::compute(&format!("{seed}-{i}")))
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        }
    }

    fn get(&self, row: usize, column: usize) -> bool {
        // This is backwards from the problem description but that doesn't
        // actually matter for computing adjacency
        (self.disk[row] & 1 << column) != 0
    }

    fn set(&mut self, row: usize, column: usize) {
        self.disk[row] |= 1 << column;
    }

    #[allow(unused)]
    fn debug_print(&self) {
        for row in 0..128 {
            for col in (0..128).rev() {
                if self.get(row, col) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }
}

fn parse_input(input: &str) -> &str {
    input.trim()
}

fn star1(input: &str) -> u32 {
    let grid = DiskGrid::from_knot_hash(input);
    grid.disk.iter().map(|i| i.count_ones()).sum()
}

fn star2(input: &str) -> u32 {
    let grid = DiskGrid::from_knot_hash(input);
    let mut regions = 0;

    let nodes_to_check = (0..128).flat_map(|i| (0..128).map(move |j| (i, j)));

    let mut explored = DiskGrid::new();

    for (row, column) in nodes_to_check {
        if !grid.get(row, column) || explored.get(row, column) {
            continue;
        }

        regions += 1;
        let mut frontier = vec![(row, column)];
        while let Some((row, column)) = frontier.pop() {
            if !grid.get(row, column) {
                continue;
            }

            if !explored.get(row, column) {
                explored.set(row, column);

                if row > 0 {
                    frontier.push((row - 1, column));
                }
                if row + 1 < 128 {
                    frontier.push((row + 1, column));
                }
                if column > 0 {
                    frontier.push((row, column - 1));
                }
                if column + 1 < 128 {
                    frontier.push((row, column + 1));
                }
            }
        }
    }

    regions
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
