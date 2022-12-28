#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! itertools = "0.10"
//! ```

// This is the travelling salesman again?
// Again n = 8. The cyclic nature means that technically we only need to check
// 7! arrangements due to the cyclicity but checking 8! is easier

use itertools::Itertools;
use std::collections::HashMap;

fn parse_input(input: String) -> Vec<i32> {
    fn extract(s: &str) -> (&str, &str, i32) {
        let split: Vec<_> = s.split_ascii_whitespace().collect();
        let from = split[0];
        let to = split[10].strip_suffix(".").unwrap();
        let abs_points = split[3].parse::<i32>().unwrap();
        let points = if split[2] == "gain" {
            abs_points
        } else {
            -abs_points
        };
        (from, to, points)
    }

    let mut names = HashMap::new();
    for line in input.lines() {
        let (from, to, points) = extract(line);
        if !names.contains_key(from) {
            names.insert(from, names.len());
        }
        if !names.contains_key(to) {
            names.insert(to, names.len());
        }
    }

    let num_people = names.len();
    let mut adjacency_matrix = vec![0; num_people * num_people];

    for line in input.lines() {
        let (from, to, points) = extract(line);
        let from = names[from];
        let to = names[to];
        adjacency_matrix[from * num_people + to] = points;
    }

    adjacency_matrix
}

fn star1(input: &[i32]) -> i32 {
    let num_people = (input.len() as f64).sqrt() as usize;

    (0..num_people)
        .permutations(num_people)
        .map(|permutation| {
            (0..permutation.len())
                .map(|i| {
                    let a = permutation[i];
                    let b = permutation[(i + 1) % permutation.len()];
                    input[a * num_people + b] + input[b * num_people + a]
                })
                .sum::<i32>()
        })
        .max()
        .unwrap()
}

fn star2(input: &[i32]) -> i32 {
    let num_people = (input.len() as f64).sqrt() as usize;
    let mut new_adjacency_matrix = Vec::with_capacity((num_people + 1) * (num_people + 1));

    for i in 0..num_people {
        new_adjacency_matrix.extend_from_slice(&input[i * num_people..(i + 1) * num_people]);
        new_adjacency_matrix.push(0);
    }
    for _ in 0..num_people + 1 {
        new_adjacency_matrix.push(0);
    }

    star1(&new_adjacency_matrix)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
