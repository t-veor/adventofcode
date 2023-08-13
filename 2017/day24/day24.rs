#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

struct BridgeState {
    pieces: Vec<(i32, i32)>,
    used_pieces: Vec<bool>,
    length: usize,
    end: i32,
}

impl BridgeState {
    fn new(pieces: &[(i32, i32)]) -> Self {
        Self {
            pieces: pieces.to_vec(),
            used_pieces: vec![false; pieces.len()],
            length: 0,
            end: 0,
        }
    }

    fn search_all(&mut self, callback: &mut impl FnMut(usize, i32)) {
        let sum = self
            .used_pieces
            .iter()
            .enumerate()
            .map(|(i, &used)| {
                if used {
                    let piece = self.pieces[i];
                    piece.0 + piece.1
                } else {
                    0
                }
            })
            .sum();

        callback(self.length, sum);

        let prev_end = self.end;

        for i in 0..self.pieces.len() {
            if self.used_pieces[i] {
                continue;
            }

            let piece = self.pieces[i];
            if piece.0 == self.end {
                self.end = piece.1
            } else if piece.1 == self.end {
                self.end = piece.0
            } else {
                continue;
            }

            self.used_pieces[i] = true;
            self.length += 1;

            self.search_all(callback);

            self.used_pieces[i] = false;
            self.end = prev_end;
            self.length -= 1;
        }
    }
}

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    input
        .lines()
        .map(|line| {
            let (a, b) = line.trim().split_once('/').unwrap();
            (a.parse().unwrap(), b.parse().unwrap())
        })
        .collect()
}

fn star1(input: &[(i32, i32)]) -> i32 {
    let mut bridge_state = BridgeState::new(input);

    let mut all_sums = Vec::new();
    bridge_state.search_all(&mut |_, sum| all_sums.push(sum));

    all_sums.iter().copied().max().unwrap()
}

fn star2(input: &[(i32, i32)]) -> i32 {
    let mut bridge_state = BridgeState::new(input);

    let mut all_bridges = Vec::new();
    bridge_state.search_all(&mut |length, sum| all_bridges.push((length, sum)));

    all_bridges.iter().copied().max().unwrap().1
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
