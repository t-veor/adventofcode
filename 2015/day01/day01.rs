#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: String) -> String {
    input
}

fn star1(input: &str) -> i32 {
    input
        .as_bytes()
        .iter()
        .map(|c| match c {
            b'(' => 1,
            b')' => -1,
            _ => 0,
        })
        .sum()
}

fn star2(input: &str) -> usize {
    let mut floor = 0;
    for (i, c) in input.as_bytes().iter().enumerate() {
        match c {
            b'(' => floor += 1,
            b')' => floor -= 1,
            _ => (),
        }
        if floor < 0 {
            return i + 1;
        }
    }

    panic!("Santa never enters the basement")
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
