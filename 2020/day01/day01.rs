#!/usr/bin/env run-cargo-script

fn star1(input: &[i32]) -> i32 {
    for i in 0..input.len() {
        for j in i+1..input.len() {
            let (a, b) = (input[i], input[j]);
            if a + b == 2020 {
                return a * b;
            }
        }
    }
    unreachable!()
}

fn star2(input: &[i32]) -> i32 {
    for i in 0..input.len() {
        for j in i+1..input.len() {
            for k in j+1..input.len() {
                let (a, b, c) = (input[i], input[j], input[k]);
                if a + b + c == 2020 {
                    return a * b * c;
                }
            }
        }
    }
    unreachable!()
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
