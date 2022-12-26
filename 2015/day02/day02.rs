#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: String) -> Vec<(i32, i32, i32)> {
    input
        .lines()
        .map(|line| {
            let mut iter = line.split('x').map(|i| i.parse().unwrap());
            (
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
            )
        })
        .collect()
}

fn star1(input: &[(i32, i32, i32)]) -> i32 {
    input
        .iter()
        .map(|(l, w, h)| {
            let a = l * w;
            let b = w * h;
            let c = l * h;
            2 * (a + b + c) + a.min(b).min(c)
        })
        .sum()
}

fn star2(input: &[(i32, i32, i32)]) -> i32 {
    input
        .iter()
        .map(|(l, w, h)| {
            let a = l + w;
            let b = w + h;
            let c = l + h;
            2 * a.min(b).min(c) + l * w * h
        })
        .sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
