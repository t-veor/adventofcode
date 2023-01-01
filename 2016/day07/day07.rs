#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashSet;

fn parse_input(input: &str) -> Vec<&str> {
    input.lines().collect()
}

fn supports_tls(addr: &str) -> bool {
    let mut has_abba = false;
    let mut inside_square_brackets = false;

    let addr = addr.as_bytes();
    for i in 0..addr.len() {
        if addr[i] == b'[' {
            inside_square_brackets = true;
        } else if addr[i] == b']' {
            inside_square_brackets = false;
        } else if i >= 3 {
            let (a, b, c, d) = (addr[i - 3], addr[i - 2], addr[i - 1], addr[i]);
            if a == d && b == c && a != b && !b"[]".contains(&a) && !b"[]".contains(&b) {
                if inside_square_brackets {
                    return false;
                } else {
                    has_abba = true;
                }
            }
        }
    }

    has_abba
}

fn supports_ssl(addr: &str) -> bool {
    let mut aba_sequences = HashSet::new();
    let mut bab_sequences = HashSet::new();

    let mut inside_square_brackets = false;

    let addr = addr.as_bytes();
    for i in 0..addr.len() {
        if addr[i] == b'[' {
            inside_square_brackets = true;
        } else if addr[i] == b']' {
            inside_square_brackets = false;
        } else if i >= 2 {
            let (a, b, c) = (addr[i - 2], addr[i - 1], addr[i]);
            if a == c && a != b && !b"[]".contains(&a) && !b"[]".contains(&b) {
                if inside_square_brackets {
                    bab_sequences.insert((b, a));
                } else {
                    aba_sequences.insert((a, b));
                }
            }
        }
    }

    !aba_sequences.is_disjoint(&bab_sequences)
}

fn star1(input: &[&str]) -> usize {
    input.iter().filter(|i| supports_tls(i)).count()
}

fn star2(input: &[&str]) -> usize {
    input.iter().filter(|i| supports_ssl(i)).count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
