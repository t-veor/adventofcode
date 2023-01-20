#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn step(state: &[bool]) -> Vec<bool> {
    let mut new_state = Vec::with_capacity(state.len());

    for i in 0..state.len() {
        let left = if i == 0 { false } else { state[i - 1] };
        let right = if i == state.len() - 1 {
            false
        } else {
            state[i + 1]
        };

        new_state.push(left ^ right)
    }

    new_state
}

fn parse_input(input: &str) -> Vec<bool> {
    input
        .as_bytes()
        .iter()
        .filter_map(|c| match c {
            b'^' => Some(true),
            b'.' => Some(false),
            _ => None,
        })
        .collect()
}

fn count_safe(mut state: Vec<bool>, rows: usize) -> usize {
    if rows == 0 {
        return 0;
    }

    let mut count = state.iter().filter(|&&x| !x).count();
    for _ in 0..rows - 1 {
        state = step(&state);
        count += state.iter().filter(|&&x| !x).count();
    }

    count
}

fn star1(input: &[bool]) -> usize {
    count_safe(input.to_vec(), 40)
}

fn star2(input: &[bool]) -> usize {
    count_safe(input.to_vec(), 400000)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
