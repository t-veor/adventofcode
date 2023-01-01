#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: String) -> Vec<[i32; 3]> {
    input
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|i| i.parse().unwrap())
                .collect::<Vec<i32>>()
                .try_into()
                .unwrap()
        })
        .collect()
}

fn star1(input: &[[i32; 3]]) -> usize {
    input
        .iter()
        .filter(|&&sides| {
            let mut sides = sides;
            sides.sort_unstable();
            sides[0] + sides[1] > sides[2]
        })
        .count()
}

fn star2(input: &[[i32; 3]]) -> usize {
    let flattened: Vec<_> = (0..3)
        .flat_map(|i| input.iter().map(move |row| row[i]))
        .collect();
    flattened
        .chunks_exact(3)
        .filter(|&chunk| {
            let mut sides: [i32; 3] = chunk.try_into().unwrap();
            sides.sort_unstable();
            sides[0] + sides[1] > sides[2]
        })
        .count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
