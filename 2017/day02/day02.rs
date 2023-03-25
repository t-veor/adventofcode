#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    input
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|i| i.parse().unwrap())
                .collect()
        })
        .collect()
}

fn checksum(row: &[i32]) -> i32 {
    match (row.iter().min(), row.iter().max()) {
        (Some(min), Some(max)) => max - min,
        _ => 0,
    }
}

fn star1(input: &[Vec<i32>]) -> i32 {
    input.iter().map(|row| checksum(row)).sum()
}

fn evenly_divisible_result(row: &[i32]) -> i32 {
    for i in 0..row.len() {
        for j in 0..row.len() {
            if i == j {
                continue;
            }

            if row[i] % row[j] == 0 {
                return row[i] / row[j];
            }
        }
    }

    0
}

fn star2(input: &[Vec<i32>]) -> i32 {
    input.iter().map(|row| evenly_divisible_result(row)).sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
