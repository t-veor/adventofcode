#!/usr/bin/env run-cargo-script
use std::collections::HashMap;

fn monotonic(s: &str) -> bool {
    for (i, j) in s.chars().zip(s.chars().skip(1)) {
        if i > j {
            return false;
        }
    }
    true
}

// Yes, making a hashmap is overkill for this problem
fn freqs(s: &str) -> HashMap<char, i32> {
    let mut res = HashMap::new();
    for i in s.chars() {
        res.entry(i).and_modify(|e| *e += 1).or_insert(1);
    }
    res
}

fn hasConsecutive(s: &str) -> bool {
    freqs(s).iter().any(|(_, i)| *i >= 2)
}

fn hasAdjacent(s: &str) -> bool {
    freqs(s).iter().any(|(_, i)| *i == 2)
}

fn star1(start: i32, end: i32) -> usize {
    (start..=end)
        .map(|i| format!("{}", i))
        .filter(|i| monotonic(i))
        .filter(|i| hasConsecutive(i))
        .count()
}

fn star2(start: i32, end: i32) -> usize {
    (start..=end)
        .map(|i| format!("{}", i))
        .filter(|i| monotonic(i))
        .filter(|i| hasAdjacent(i))
        .count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let file_contents = std::fs::read_to_string(filename).unwrap();
    let mut input = file_contents.trim().split("-");
    let start = input.next().unwrap().parse().unwrap();
    let end = input.next().unwrap().parse().unwrap();

    println!("{}", star1(start, end));
    println!("{}", star2(start, end));
}
