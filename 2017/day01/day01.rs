#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> Vec<i32> {
    input
        .trim()
        .as_bytes()
        .iter()
        .map(|c| (*c as u8 - b'0') as i32)
        .collect()
}

fn star1(input: &[i32]) -> i32 {
    let mut sum = 0;
    for i in 0..input.len() {
        let j = (i + 1) % input.len();
        if input[i] == input[j] {
            sum += input[i];
        }
    }
    sum
}

fn star2(input: &[i32]) -> i32 {
    let mut sum = 0;
    let offset = input.len() / 2;
    for i in 0..input.len() {
        let j = (i + offset) % input.len();
        if input[i] == input[j] {
            sum += input[i];
        }
    }
    sum
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
