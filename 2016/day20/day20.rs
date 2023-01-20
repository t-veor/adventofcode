#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

// It's off-by-one hell, hooray

// std::ops::RangeInclusive has extra fields for iteratory stuff, just use my
// own struct
#[derive(Debug, Copy, Clone)]
struct InclusiveRange {
    start: u32,
    end: u32,
}

fn sort_and_merge_ranges(mut ranges: Vec<InclusiveRange>) -> Vec<InclusiveRange> {
    ranges.sort_unstable_by_key(|range| range.start);

    let mut i = 0;
    for j in 1..ranges.len() {
        if ranges[i].end == u32::MAX || ranges[j].start <= ranges[i].end + 1 {
            ranges[i].end = ranges[i].end.max(ranges[j].end);
        } else {
            i += 1;
            ranges[i] = ranges[j].clone();
        }
    }
    ranges.truncate(i + 1);

    ranges
}

fn invert_ranges(sorted_ranges: &[InclusiveRange]) -> Vec<InclusiveRange> {
    let mut result = Vec::with_capacity(sorted_ranges.len() + 1);

    let mut x = 0;
    for InclusiveRange { start, end } in sorted_ranges {
        if *start > 0 && x <= start - 1 {
            result.push(InclusiveRange {
                start: x,
                end: start - 1,
            });
        }
        x = end.saturating_add(1);
    }

    if let Some(InclusiveRange { end, .. }) = sorted_ranges.last() {
        if *end < u32::MAX {
            result.push(InclusiveRange {
                start: end + 1,
                end: u32::MAX,
            });
        }
    }

    result
}

fn parse_input(input: &str) -> Vec<InclusiveRange> {
    input
        .lines()
        .map(|line| {
            let (start, end) = line.split_once('-').unwrap();
            let start = start.parse().unwrap();
            let end = end.parse().unwrap();
            InclusiveRange { start, end }
        })
        .collect()
}

fn star1(input: &[InclusiveRange]) -> u32 {
    let sorted_ranges = sort_and_merge_ranges(input.to_vec());
    let inverted_ranges = invert_ranges(&sorted_ranges);
    inverted_ranges.first().unwrap().start
}

fn star2(input: &[InclusiveRange]) -> u64 {
    // Returns a u64 as potentially there could be 2^32 available addresses,
    // which is too large for a u32
    let sorted_ranges = sort_and_merge_ranges(input.to_vec());
    let inverted_ranges = invert_ranges(&sorted_ranges);
    inverted_ranges
        .iter()
        .map(|range| range.end as u64 - range.start as u64 + 1)
        .sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
