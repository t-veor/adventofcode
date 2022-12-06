#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn count_bits(input: &[Vec<u8>], bit: usize) -> (u32, u32) {
    let mut ones = 0;
    for i in input {
        ones += i[bit] as u32;
    }
    let zeroes = input.len() as u32 - ones;
    (zeroes, ones)
}

fn from_binary(v: &[u8]) -> u32 {
    let mut n = 0;
    for bit in v {
        n = 2 * n + *bit as u32
    }
    n
}

fn filter_by_bit(input: &mut Vec<Vec<u8>>, bit: usize, negate: bool) {
    let (zeroes, ones) = count_bits(input, bit);

    let criteria = if negate ^ (ones >= zeroes) { 1 } else { 0 };
    input.retain(|v| v[bit] == criteria)
}

fn filter_until_one(mut input: Vec<Vec<u8>>, negate: bool) -> Vec<u8> {
    for bit in 0.. {
        filter_by_bit(&mut input, bit, negate);
        if input.len() <= 1 {
            break;
        }
    }

    input.pop().unwrap()
}

fn star1(input: &[Vec<u8>]) -> u32 {
    let num_bits = input[0].len();

    let gamma: Vec<_> = (0..num_bits)
        .map(|i| {
            let (zeroes, ones) = count_bits(input, i);
            (ones >= zeroes) as u8
        })
        .collect();
    let gamma = from_binary(&gamma);
    let epsilon = ((1 << num_bits) - 1) & !gamma;

    gamma * epsilon
}

fn star2(input: &[Vec<u8>]) -> u32 {
    let oxygen_binary = filter_until_one(input.iter().cloned().collect(), false);
    let co2_binary = filter_until_one(input.iter().cloned().collect(), true);

    let oxygen = from_binary(&oxygen_binary);
    let co2 = from_binary(&co2_binary);

    oxygen * co2
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input: Vec<_> = std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|s| s.chars().map(|c| (c == '1') as u8).collect())
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
