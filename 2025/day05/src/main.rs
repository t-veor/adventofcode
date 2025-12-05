use std::ops::RangeInclusive;

use utils::read_input_file;

fn parse_file(input: &str) -> (Vec<RangeInclusive<u64>>, Vec<u64>) {
    let (ranges, values) = input.split_once("\n\n").unwrap();

    let ranges = ranges
        .lines()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            let (a, b) = (a.parse().unwrap(), b.parse().unwrap());
            a..=b
        })
        .collect();
    let values = values.lines().map(|line| line.parse().unwrap()).collect();

    (ranges, values)
}

#[derive(Debug, Default)]
struct IntervalSet {
    sorted_ranges: Vec<RangeInclusive<u64>>,
}

impl IntervalSet {
    pub fn new(ranges: &[RangeInclusive<u64>]) -> Self {
        let mut ranges = ranges.to_vec();
        ranges.sort_by_key(|r| *r.start());

        let mut i = 0;
        for j in 0..ranges.len() {
            if ranges[j].start() <= ranges[i].end() {
                // Merge
                ranges[i] = *ranges[i].start()..=(*ranges[j].end()).max(*ranges[i].end())
            } else {
                // Write new
                i += 1;
                ranges[i] = ranges[j].clone();
            }
        }

        ranges.truncate(i + 1);

        Self {
            sorted_ranges: ranges,
        }
    }

    pub fn size(&self) -> u64 {
        self.sorted_ranges
            .iter()
            .map(|r| r.end() - r.start() + 1)
            .sum()
    }
}

fn star1(ranges: &[RangeInclusive<u64>], values: &[u64]) -> usize {
    values
        .iter()
        .filter(|&x| ranges.iter().any(|r| r.contains(x)))
        .count()
}

fn star2(ranges: &[RangeInclusive<u64>]) -> u64 {
    IntervalSet::new(ranges).size()
}

fn main() {
    let input = read_input_file!();
    let (ranges, values) = parse_file(&input);

    println!("{}", star1(&ranges, &values));
    println!("{}", star2(&ranges));
}
