#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: String) -> Vec<String> {
    input.lines().map(|s| s.to_owned()).collect()
}

fn is_nice_1(s: &str) -> bool {
    let bytes = s.as_bytes();
    let num_vowels = bytes.iter().filter(|c| b"aeiou".contains(c)).count();

    let mut has_double_letter = false;
    for i in 1..bytes.len() {
        if bytes[i - 1] == bytes[i] {
            has_double_letter = true;
            break;
        }
    }

    let has_naughty_string = ["ab", "cd", "pq", "xy"].iter().any(|p| s.contains(p));

    num_vowels >= 3 && has_double_letter && !has_naughty_string
}

fn star1(input: &[String]) -> usize {
    input.iter().filter(|s| is_nice_1(s)).count()
}

fn is_nice_2(s: &str) -> bool {
    let bytes = s.as_bytes();

    // The strings are not that long. I'm just going to implement a quadratic
    // search because it's easier (and probably faster on these small strings)
    let mut has_doubled_pair = false;
    for i in 1..bytes.len() {
        for j in i + 2..bytes.len() {
            if bytes[i - 1] == bytes[j - 1] && bytes[i] == bytes[j] {
                has_doubled_pair = true;
                break;
            }
        }
    }

    let mut has_repeat_with_letter = false;
    for i in 2..bytes.len() {
        if bytes[i - 2] == bytes[i] {
            has_repeat_with_letter = true;
            break;
        }
    }

    has_doubled_pair && has_repeat_with_letter
}

fn star2(input: &[String]) -> usize {
    input.iter().filter(|s| is_nice_2(s)).count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
