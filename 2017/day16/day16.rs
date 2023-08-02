#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum DanceMove {
    Spin(usize),
    Exchange(usize, usize),
    Partner(u8, u8),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Dancers {
    dancers: Vec<u8>,
}

impl Dancers {
    fn new() -> Self {
        Self {
            dancers: (b'a'..=b'p').collect(),
        }
    }

    fn perform_move(&mut self, m: DanceMove) {
        match m {
            DanceMove::Spin(x) => self.dancers.rotate_right(x),
            DanceMove::Exchange(i, j) => {
                let tmp = self.dancers[i];
                self.dancers[i] = self.dancers[j];
                self.dancers[j] = tmp;
            }
            DanceMove::Partner(a, b) => {
                let i = self.dancers.iter().position(|&x| x == a).unwrap();
                let j = self.dancers.iter().position(|&x| x == b).unwrap();
                let tmp = self.dancers[i];
                self.dancers[i] = self.dancers[j];
                self.dancers[j] = tmp;
            }
        }
    }

    fn perform_sequence(&mut self, moves: &[DanceMove]) {
        for &m in moves {
            self.perform_move(m);
        }
    }

    fn output(&self) -> String {
        String::from_utf8_lossy(&self.dancers).to_string()
    }
}

fn parse_input(input: &str) -> Vec<DanceMove> {
    fn parse_move(m: &str) -> DanceMove {
        match &m[0..1] {
            "s" => DanceMove::Spin(m[1..].parse().unwrap()),
            "x" => {
                let (a, b) = m[1..].split_once('/').unwrap();
                DanceMove::Exchange(a.parse().unwrap(), b.parse().unwrap())
            }
            "p" => {
                let (a, b) = m[1..].split_once('/').unwrap();
                DanceMove::Partner(a.as_bytes()[0], b.as_bytes()[0])
            }
            _ => unreachable!(),
        }
    }

    input.trim().split(',').map(parse_move).collect()
}

fn star1(input: &[DanceMove]) -> String {
    let mut dancers = Dancers::new();

    dancers.perform_sequence(input);

    dancers.output()
}

fn star2(input: &[DanceMove]) -> String {
    let mut dancers = Dancers::new();
    let mut seen = HashMap::new();
    let mut skipped = false;

    let mut i = 0;
    while i < 1_000_000_000 {
        if !skipped {
            if let Some(prev_idx) = seen.insert(dancers.clone(), i) {
                let period = i - prev_idx;
                let remaining = 1_000_000_000 - i;
                i += (remaining / period) * period;
                skipped = true;
                continue;
            }
        }

        dancers.perform_sequence(input);
        i += 1;
    }

    dancers.output()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
