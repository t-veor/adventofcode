#!/usr/bin/env run-cargo-script
fn fuel_reqs(mass: i32) -> i32 {
    mass / 3 - 2
}

fn real_fuel_reqs(mass: i32) -> i32 {
    if mass < 0 {
        0
    } else {
        let fuel = mass / 3 - 2;
        fuel + real_fuel_reqs(fuel)
    }
}

fn star1(input: &Vec<i32>) -> i32 {
    input.iter().map(|i| fuel_reqs(*i)).sum()
}

fn star2(input: &Vec<i32>) -> i32 {
    input.iter().map(|i| real_fuel_reqs(*i)).sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename)
        .unwrap()
        .split_whitespace()
        .map(|i| i.parse().unwrap())
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
