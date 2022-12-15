#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! regex = "1"
//! ```

use regex::Regex;
use std::{collections::HashSet, ops::Range};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct IVec2 {
    x: i32,
    y: i32,
}

impl IVec2 {
    fn manhanttan_distance(&self, other: &Self) -> u32 {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }
}

#[derive(Debug, Clone)]
struct Sensor {
    position: IVec2,
    closest_beacon: IVec2,
}

impl Sensor {
    fn get_covered_range_for_y(&self, y: i32) -> Range<i32> {
        let beacon_distance = self.position.manhanttan_distance(&self.closest_beacon) as i32;
        let height_diff = y.abs_diff(self.position.y) as i32;
        let range_width_at_height = beacon_distance - height_diff;

        self.position.x - range_width_at_height..self.position.x + range_width_at_height + 1
    }
}

fn merge_ranges(mut ranges: Vec<Range<i32>>) -> Vec<Range<i32>> {
    ranges.sort_by_key(|range| range.start);

    let mut curr = 0;
    for i in 0..ranges.len() {
        if ranges[i].start <= ranges[curr].end {
            ranges[curr].end = ranges[curr].end.max(ranges[i].end);
        } else {
            curr += 1;
            ranges[curr] = ranges[i].clone();
        }
    }
    ranges.truncate(curr + 1);
    ranges
}

fn parse_input(input: String) -> Vec<Sensor> {
    let re =
        Regex::new(r"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$")
            .unwrap();

    input
        .lines()
        .map(|line| {
            let captures = re.captures(line).unwrap();
            let sensor_x = captures[1].parse().unwrap();
            let sensor_y = captures[2].parse().unwrap();
            let beacon_x = captures[3].parse().unwrap();
            let beacon_y = captures[4].parse().unwrap();

            Sensor {
                position: IVec2 {
                    x: sensor_x,
                    y: sensor_y,
                },
                closest_beacon: IVec2 {
                    x: beacon_x,
                    y: beacon_y,
                },
            }
        })
        .collect()
}

fn star1(input: &[Sensor]) -> usize {
    let height = 2000000;

    let ranges = merge_ranges(
        input
            .iter()
            .filter_map(|sensor| {
                let range = sensor.get_covered_range_for_y(height);
                (!range.is_empty()).then_some(range)
            })
            .collect(),
    );

    let covered_positions = ranges.iter().map(|range| range.len()).sum::<usize>();
    let already_seen_beacons = input
        .iter()
        .filter_map(|sensor| (sensor.closest_beacon.y == height).then_some(sensor.closest_beacon))
        .collect::<HashSet<_>>()
        .len();

    covered_positions.saturating_sub(already_seen_beacons)
}

fn star2(input: &[Sensor]) -> u64 {
    for y in 0..4000000 {
        let ranges = merge_ranges(
            input
                .iter()
                .filter_map(|sensor| {
                    let range = sensor.get_covered_range_for_y(y);
                    (!range.is_empty()).then_some(range)
                })
                .collect(),
        );
        if ranges.len() > 1 {
            let x = ranges[0].end;
            return (x as u64) * 4000000 + y as u64;
        }
    }

    unreachable!()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
