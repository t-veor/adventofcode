#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> Vec<Vec<&str>> {
    input
        .lines()
        .map(|line| line.split_ascii_whitespace().collect())
        .collect()
}

fn all_different<T: Eq>(words: &[T]) -> bool {
    // None of the passphrases are very long. It's probably just faster to do a
    // little bit of extra number crunching
    for i in 0..words.len() {
        for j in 0..i {
            if words[i] == words[j] {
                return false;
            }
        }
    }

    true
}

fn star1(input: &[Vec<&str>]) -> usize {
    input
        .iter()
        .filter(|passphrase| all_different(&passphrase))
        .count()
}

fn freqs(word: &str) -> [u8; 26] {
    let mut freqs = [0; 26];
    for c in word.as_bytes() {
        if (b'a'..=b'z').contains(c) {
            let idx = (c - b'a') as usize;
            freqs[idx] += 1;
        }
    }

    freqs
}

fn star2(input: &[Vec<&str>]) -> usize {
    input
        .iter()
        .filter(|passphrase| {
            let passphrase_freqs: Vec<_> = passphrase.iter().map(|word| freqs(word)).collect();
            all_different(&passphrase_freqs)
        })
        .count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
