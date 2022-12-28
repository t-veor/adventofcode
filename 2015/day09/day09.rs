#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! itertools = "0.10"
//! ```

// Literally just the travelling salesman problem?
// Inspecting the input reveals there to be only 8 locations. 8! = 40320, which
// is very feasible to brute force through.

use itertools::Itertools;
use std::collections::HashMap;

fn parse_input(input: String) -> Vec<i32> {
    let mut locations = HashMap::new();
    for line in input.lines() {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        if !locations.contains_key(split[0]) {
            locations.insert(split[0].to_owned(), locations.len());
        }
        if !locations.contains_key(split[2]) {
            locations.insert(split[2].to_owned(), locations.len());
        }
    }

    let num_locations = locations.len();
    let mut distance_matrix = vec![0; num_locations * num_locations];
    for line in input.lines() {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        let from = locations[split[0]];
        let to = locations[split[2]];
        let distance = split[4].parse().unwrap();
        distance_matrix[from * num_locations + to] = distance;
        distance_matrix[to * num_locations + from] = distance;
    }

    distance_matrix
}

fn star1(distances: &[i32]) -> i32 {
    let num_locations = (distances.len() as f64).sqrt() as usize;

    (0..num_locations)
        .permutations(num_locations)
        .map(|permutation| {
            permutation
                .into_iter()
                .tuple_windows()
                .map(|(from, to)| distances[from * num_locations + to])
                .sum()
        })
        .min()
        .unwrap()
}

fn star2(distances: &[i32]) -> i32 {
    let num_locations = (distances.len() as f64).sqrt() as usize;

    (0..num_locations)
        .permutations(num_locations)
        .map(|permutation| {
            permutation
                .into_iter()
                .tuple_windows()
                .map(|(from, to)| distances[from * num_locations + to])
                .sum()
        })
        .max()
        .unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
