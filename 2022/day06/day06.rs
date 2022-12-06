#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```
fn parse_input(input: &str) -> &[u8] {
    input.trim().as_bytes()
}

fn all_different(input: &[u8]) -> bool {
    // O(n^2) algorithm which should be faster on small arrays
    for i in 0..input.len() {
        for j in i + 1..input.len() {
            if input[i] == input[j] {
                return false;
            }
        }
    }

    true
}

fn start_of_packet(input: &[u8], count: usize) -> Option<usize> {
    for i in 0..=input.len().saturating_sub(count) {
        let potential_marker = &input[i..i + count];
        if all_different(potential_marker) {
            return Some(i);
        }
    }
    None
}

fn star1(input: &[u8]) -> usize {
    start_of_packet(input, 4).unwrap() + 4
}

fn star2(input: &[u8]) -> usize {
    start_of_packet(input, 14).unwrap() + 14
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
