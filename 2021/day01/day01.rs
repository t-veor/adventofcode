#!/usr/bin/env run-cargo-script

fn star1(input: &[i32]) -> i32 {
    input
        .windows(2)
        .map(|pair| {
            let (x, y) = (pair[0], pair[1]);
            (y > x) as i32
        })
        .sum()
}

fn star2(input: &[i32]) -> i32 {
    let windows: Vec<_> = input.windows(3).collect();
    windows
        .windows(2)
        .map(|pair| {
            let (xs, ys) = (pair[0], pair[1]);
            let x: i32 = xs.iter().sum();
            let y: i32 = ys.iter().sum();
            (y > x) as i32
        })
        .sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input: Vec<_> = std::fs::read_to_string(filename)
        .unwrap()
        .split_whitespace()
        .map(|i| i.parse().unwrap())
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
