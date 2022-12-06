#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```
fn parse_input(input: String) -> Vec<(i32, i32)> {
    input
        .lines()
        .map(|line| {
            let line = line.as_bytes();
            let their_move = line[0] - b'A';
            let variable = line[2] - b'X';
            (their_move as _, variable as _)
        })
        .collect()
}

fn star1(input: &[(i32, i32)]) -> i32 {
    input
        .iter()
        .copied()
        .map(|(their_move, your_move)| {
            // maps a win to 2, draw to 1, and loss to 0
            let outcome = (your_move - their_move + 1).rem_euclid(3);

            your_move + 1 + outcome * 3
        })
        .sum()
}

fn star2(input: &[(i32, i32)]) -> i32 {
    input
        .iter()
        .copied()
        .map(|(their_move, outcome)| {
            // Since we know that your_move - their_move + 1 `equiv` outcome (mod 3)
            // it follows that your_move `equiv` outcome + their_move - 1 (mod 3)
            let your_move = (outcome + their_move - 1).rem_euclid(3);

            your_move + 1 + outcome * 3
        })
        .sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
