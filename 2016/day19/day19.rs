#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn josephus(n: u32) -> u32 {
    if n == 0 {
        0
    } else {
        let highest_bit = 1 << (u32::BITS - n.leading_zeros() - 1);
        2 * (n - highest_bit) + 1
    }
}

fn modified_josephus(n: u32) -> u32 {
    if n == 0 {
        0
    } else {
        let mut highest_power_of_three = 1;
        while highest_power_of_three * 3 < n {
            highest_power_of_three *= 3;
        }
        let next_power_of_three = highest_power_of_three * 3;
        2 * n - highest_power_of_three - ((next_power_of_three + highest_power_of_three) / 2).min(n)
    }
}

fn parse_input(input: &str) -> u32 {
    input.trim().parse().unwrap()
}

fn star1(input: &u32) -> u32 {
    josephus(*input)
}

fn star2(input: &u32) -> u32 {
    modified_josephus(*input)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
