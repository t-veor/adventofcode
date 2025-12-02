use std::ops::RangeInclusive;

use utils::read_input_file;

fn parse_input(input: &str) -> Vec<RangeInclusive<u64>> {
    input
        .split(',')
        .map(|s| s.trim())
        .map(|s| {
            let (a, b) = s.split_once('-').unwrap();
            a.parse().unwrap()..=b.parse().unwrap()
        })
        .collect()
}

fn number_len(x: u64) -> u32 {
    x.ilog10() + 1
}

fn has_repeat_with_length(mut x: u64, len: u32) -> bool {
    if len == 0 {
        return false;
    }

    let base = 10u64.pow(len);

    let initial_substring = x % base;
    if initial_substring < 10u64.pow(len - 1) {
        // The initial `len` digits has a leading 0, so this can't possibly be a candidate
        return false;
    }

    while x > 0 {
        if x % base != initial_substring {
            return false;
        }

        x /= base;
    }

    true
}

fn is_double_repeat(x: u64) -> bool {
    has_repeat_with_length(x, number_len(x) / 2)
}

fn star1(ranges: &[RangeInclusive<u64>]) -> u64 {
    ranges
        .iter()
        .flat_map(|range| range.clone())
        .filter(|&x| is_double_repeat(x))
        .sum()
}

fn has_any_repeat(x: u64) -> bool {
    (1..=number_len(x) / 2).any(|len| has_repeat_with_length(x, len))
}

fn star2(ranges: &[RangeInclusive<u64>]) -> u64 {
    ranges
        .iter()
        .flat_map(|range| range.clone())
        .filter(|&x| has_any_repeat(x))
        .sum()
}

fn main() {
    let input = read_input_file!();
    let ranges = parse_input(&input);

    println!("{}", star1(&ranges));
    println!("{}", star2(&ranges));
}
