#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

// Obviously, the house n receives 10 * sigma(n) presents, where sigma(n) is
// equal to the sum of all divisors of n. Knowing this fact doesn't give us a
// very fast algorithm to compute it though -- sigma(n) can be computed in
// O(sqrt(n)), but since sigma(n) is not monotonic we'd need to check all
// numbers less than the input leading to the runtime being O(n^1.5). I couldn't
// think of a faster way of computing this, but instead we could literally just
// perform the dumb way of literally allocating an n-sized array and performing
// the dumb method of simulating each elves and be done also in O(n log n) (as
// we'd need to perform n + n/2 + n/3 + ... + n/n operations, which is n * H(n)
// where H(n) is the nth harmonic number and H(n) = O(log n).)

fn parse_input(input: String) -> usize {
    input.trim().parse().unwrap()
}

fn star1(input: &usize) -> usize {
    let required = input / 10;

    let mut presents = vec![0; required];
    for elf in 1..required {
        for i in (0..required).step_by(elf).skip(1) {
            presents[i] += elf;
        }
    }

    presents.iter().position(|&x| x >= required).unwrap()
}

// Okay, well, vindicated, part 2 isn't the sigma function at all

fn star2(input: &usize) -> usize {
    let required = (*input as f64 / 11.0).ceil() as usize;

    let mut presents = vec![0; required];
    for elf in 1..required {
        for i in (0..required).step_by(elf).skip(1).take(50) {
            presents[i] += elf;
        }
    }

    presents.iter().position(|&x| x * 11 >= *input).unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
