#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> Vec<u32> {
    input
        .split_ascii_whitespace()
        .map(|i| i.parse().unwrap())
        .collect()
}

fn step(mut blocks: Vec<u32>) -> Vec<u32> {
    let mut max_idx = 0;
    for i in 1..blocks.len() {
        if blocks[i] > blocks[max_idx] {
            max_idx = i;
        }
    }

    let block_count = blocks.len() as u32;
    let quot = blocks[max_idx] / block_count;
    let rem = blocks[max_idx] % block_count;
    blocks[max_idx] = 0;

    for i in 0..blocks.len() {
        blocks[i] += quot;
        if (i + blocks.len() - max_idx - 1) % blocks.len() < rem as usize {
            blocks[i] += 1;
        }
    }

    blocks
}

fn floyd<S: Eq + Clone>(next: impl Fn(S) -> S, start: S) -> (u32, u32) {
    let mut tortoise = next(start.clone());
    let mut hare = next(tortoise.clone());

    while tortoise != hare {
        tortoise = next(tortoise);
        hare = next(next(hare));
    }

    let mut mu = 0;
    tortoise = start;
    while tortoise != hare {
        tortoise = next(tortoise);
        hare = next(hare);
        mu += 1;
    }

    let mut lambda = 1;
    hare = next(tortoise.clone());
    while tortoise != hare {
        hare = next(hare);
        lambda += 1;
    }

    (mu, lambda)
}

fn star1(input: &[u32]) -> u32 {
    let (cycle_start, cycle_len) = floyd(step, input.to_vec());
    cycle_start + cycle_len
}

fn star2(input: &[u32]) -> u32 {
    floyd(step, input.to_vec()).1
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
