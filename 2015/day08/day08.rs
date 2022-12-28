#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: String) -> Vec<String> {
    input.lines().map(|s| s.to_owned()).collect()
}

fn count_chars(s: &str) -> usize {
    let bytes = s.as_bytes();
    let mut i = 1;
    let mut count = 0;
    while i < bytes.len().saturating_sub(1) {
        count += 1;
        if bytes[i] == b'\\' {
            match bytes.get(i + 1) {
                Some(b'"') | Some(b'\\') => i += 1,
                Some(b'x') => i += 3,
                _ => (),
            }
        }
        i += 1;
    }
    count
}

fn encoded_length(s: &str) -> usize {
    let mut count = 2;
    for byte in s.as_bytes() {
        count += match byte {
            b'"' | b'\\' => 2,
            _ => 1,
        };
    }
    count
}

fn star1(input: &[String]) -> usize {
    input.iter().map(|s| s.len() - count_chars(s)).sum()
}

fn star2(input: &[String]) -> usize {
    input.iter().map(|s| encoded_length(s) - s.len()).sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
