#!/usr/bin/env run-cargo-script
use std::collections::HashMap;
use std::collections::HashSet;

fn parse_wire(s: &str) -> Vec<((i32, i32), i32)> {
    s.split(",")
        .map(|i| {
            let (dir, len) = i.split_at(1);
            let delta = match dir {
                "U" => (0, -1),
                "D" => (0, 1),
                "L" => (-1, 0),
                "R" => (1, 0),
                _ => panic!(format!("Unknown direction {}", dir)),
            };
            (delta, len.parse().unwrap())
        })
        .collect()
}

fn get_wire_positions(wire: &Vec<((i32, i32), i32)>) -> HashMap<(i32, i32), i32> {
    let mut res = HashMap::new();
    let mut x = 0;
    let mut y = 0;
    let mut steps = 0;
    for ((dx, dy), length) in wire {
        for _ in 0..*length {
            x += dx;
            y += dy;
            steps += 1;
            res.entry((x, y)).or_insert(steps);
        }
    }
    res
}

fn get_wire_intersections(
    w1: &HashMap<(i32, i32), i32>,
    w2: &HashMap<(i32, i32), i32>,
) -> HashSet<(i32, i32)> {
    let s1: HashSet<_> = w1.keys().collect();
    s1.intersection(&w2.keys().collect()).map(|i| **i).collect()
}

fn star1(intersections: &HashSet<(i32, i32)>) -> i32 {
    intersections
        .iter()
        .map(|(x, y)| x.abs() + y.abs())
        .min()
        .unwrap()
}

fn star2(
    w1: &HashMap<(i32, i32), i32>,
    w2: &HashMap<(i32, i32), i32>,
    intersections: &HashSet<(i32, i32)>,
) -> i32 {
    intersections.iter().map(|i| w1[i] + w2[i]).min().unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let file_contents = std::fs::read_to_string(filename).unwrap();
    let mut input = file_contents.split_whitespace();

    let w1 = get_wire_positions(&parse_wire(input.next().unwrap()));
    let w2 = get_wire_positions(&parse_wire(input.next().unwrap()));
    let intersections = get_wire_intersections(&w1, &w2);

    println!("{}", star1(&intersections));
    println!("{}", star2(&w1, &w2, &intersections));
}
