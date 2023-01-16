#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> Vec<bool> {
    input.trim().as_bytes().iter().map(|&i| i == b'1').collect()
}

fn expand(seed: Vec<bool>, required_length: usize) -> Vec<bool> {
    let mut data = seed;
    while data.len() < required_length {
        data.push(false);
        for i in (0..data.len() - 1).rev() {
            data.push(!data[i]);
        }
    }

    data.truncate(required_length);
    data
}

fn checksum(mut s: Vec<bool>) -> Vec<bool> {
    while s.len() % 2 == 0 {
        s = s.chunks_exact(2).map(|pair| pair[0] == pair[1]).collect();
    }

    s
}

fn star1(input: &[bool]) -> String {
    let s = expand(input.to_vec(), 272);
    let checksum = checksum(s);
    checksum
        .into_iter()
        .map(|i| if i { '1' } else { '0' })
        .collect()
}

fn star2(input: &[bool]) -> String {
    // The constant is not even that big, it's what, 35 MB of memory?
    let s = expand(input.to_vec(), 35651584);
    let checksum = checksum(s);
    checksum
        .into_iter()
        .map(|i| if i { '1' } else { '0' })
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
