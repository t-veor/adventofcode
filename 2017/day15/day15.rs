#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone)]
struct Generator {
    factor: i32,
    state: i32,
}

impl Iterator for Generator {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        self.state = (self.state as i64 * self.factor as i64 % 0x7FFF_FFFF) as i32;
        Some(self.state)
    }
}

fn parse_input(input: &str) -> (Generator, Generator) {
    let lines = input.lines().collect::<Vec<_>>();
    let a = lines[0].trim().split(' ').last().unwrap().parse().unwrap();
    let b = lines[1].trim().split(' ').last().unwrap().parse().unwrap();

    (
        Generator {
            factor: 16807,
            state: a,
        },
        Generator {
            factor: 48271,
            state: b,
        },
    )
}

fn star1(input: &(Generator, Generator)) -> u32 {
    let a = input.0.clone();
    let b = input.1.clone();

    a.take(40_000_000)
        .zip(b)
        .map(|(a, b)| if a & 0xFFFF == b & 0xFFFF { 1 } else { 0 })
        .sum()
}

fn star2(input: &(Generator, Generator)) -> i32 {
    let a = input.0.clone().filter(|i| i % 4 == 0);
    let b = input.1.clone().filter(|i| i % 8 == 0);

    a.take(5_000_000)
        .zip(b)
        .map(|(a, b)| if a & 0xFFFF == b & 0xFFFF { 1 } else { 0 })
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
