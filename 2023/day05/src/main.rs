use gcollections::ops::{set::Intersection, Bounded, Contains, Difference, Union};
use interval::{interval_set::ToIntervalSet, ops::Range, Interval, IntervalSet};
use utils::read_input_file;

#[derive(Debug)]
struct MapSegment {
    dst_start: i64,
    interval: Interval<i64>,
}

impl MapSegment {
    fn try_map(&self, x: i64) -> Option<i64> {
        if self.interval.contains(&x) {
            Some(x - self.interval.lower() + self.dst_start)
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct Map {
    segments: Vec<MapSegment>,
}

impl Map {
    fn map_single(&self, x: i64) -> i64 {
        for segment in self.segments.iter() {
            if let Some(mapped_x) = segment.try_map(x) {
                return mapped_x;
            }
        }

        return x;
    }

    fn map_set(&self, mut src_set: IntervalSet<i64>) -> IntervalSet<i64> {
        let mut dst_set = vec![].to_interval_set();

        for segment in self.segments.iter() {
            let segment_set = IntervalSet::new(segment.interval.lower(), segment.interval.upper());
            let mut intersections = src_set.intersection(&segment_set);

            // Remove intersections from the src set
            src_set = src_set.difference(&intersections);

            // Map the intersecting part
            let diff = segment.dst_start - segment.interval.lower();
            intersections = intersections + diff;

            dst_set = dst_set.union(&intersections);
        }

        src_set.union(&dst_set)
    }
}

#[derive(Debug)]
struct Input {
    seeds: Vec<i64>,
    maps: Vec<Map>,
}

impl Input {
    fn parse(input: &str) -> Self {
        let (seeds, maps) = input.split_once("\n\n").unwrap();

        let seeds = seeds
            .strip_prefix("seeds: ")
            .unwrap()
            .split_ascii_whitespace()
            .map(|i| i.parse().unwrap())
            .collect();

        let maps = maps
            .split("\n\n")
            .map(|map_data| Map {
                segments: map_data
                    .lines()
                    .skip(1)
                    .map(|line| {
                        let values: Vec<_> = line
                            .split_ascii_whitespace()
                            .map(|i| i.parse::<i64>().unwrap())
                            .collect();
                        let dst_start = values[0];
                        let src_start = values[1];
                        let range_len = values[2];

                        MapSegment {
                            dst_start,
                            interval: Interval::new(src_start, src_start + range_len - 1),
                        }
                    })
                    .collect(),
            })
            .collect();

        Self { seeds, maps }
    }
}

fn star1(input: &Input) -> i64 {
    input
        .seeds
        .iter()
        .map(|&seed| input.maps.iter().fold(seed, |x, map| map.map_single(x)))
        .min()
        .unwrap()
}

fn star2(input: &Input) -> i64 {
    let mut initial_set = input
        .seeds
        .chunks_exact(2)
        .map(|pair| (pair[0], pair[0] + pair[1] - 1))
        .collect::<Vec<_>>();
    initial_set.sort();
    let initial_set = initial_set.to_interval_set();

    let final_set = input
        .maps
        .iter()
        .fold(initial_set, |src_set, map| map.map_set(src_set));

    final_set.lower()
}

fn main() {
    let input = read_input_file!();
    let input = Input::parse(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
