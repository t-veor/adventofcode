#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

fn parse_input(input: &str) -> Vec<&str> {
    input.lines().collect()
}

fn star1(input: &[&str]) -> String {
    let num_chars = input[0].len();
    let mut freqs = vec![HashMap::new(); num_chars];

    for message in input {
        let bytes = message.as_bytes();
        for i in 0..num_chars {
            *freqs[i]
                .entry(char::from_u32(bytes[i] as u32).unwrap())
                .or_insert(0) += 1;
        }
    }

    freqs
        .into_iter()
        .map(|freqs| freqs.into_iter().max_by_key(|(_, i)| *i).unwrap().0)
        .collect()
}

fn star2(input: &[&str]) -> String {
    let num_chars = input[0].len();
    let mut freqs = vec![HashMap::new(); num_chars];

    for message in input {
        let bytes = message.as_bytes();
        for i in 0..num_chars {
            *freqs[i]
                .entry(char::from_u32(bytes[i] as u32).unwrap())
                .or_insert(0) += 1;
        }
    }

    freqs
        .into_iter()
        .map(|freqs| freqs.into_iter().min_by_key(|(_, i)| *i).unwrap().0)
        .collect()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
