#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    input
        .lines()
        .map(|line| {
            let (depth, range) = line.trim().split_once(": ").unwrap();
            (depth.parse().unwrap(), range.parse().unwrap())
        })
        .collect()
}

fn star1(input: &[(i32, i32)]) -> i32 {
    let mut severity = 0;

    for (depth, range) in input.iter().copied() {
        let period = 2 * (range - 1);
        if depth % period == 0 {
            severity += depth * range;
        }
    }

    severity
}

fn star2(input: &[(i32, i32)]) -> i32 {
    'outer: for delay in 0.. {
        for (depth, range) in input.iter().copied() {
            let period = 2 * (range - 1);
            if (depth + delay) % period == 0 {
                continue 'outer;
            }
        }

        return delay;
    }

    unreachable!()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
