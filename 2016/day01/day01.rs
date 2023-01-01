#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashSet;

#[derive(Debug, Copy, Clone)]
enum Rotation {
    Left,
    Right,
}

fn parse_input(input: String) -> Vec<(Rotation, i32)> {
    input
        .trim()
        .split(", ")
        .map(|instruction| {
            let rotation = match instruction.as_bytes()[0] {
                b'L' => Rotation::Left,
                b'R' => Rotation::Right,
                _ => unreachable!(),
            };
            let distance = instruction[1..].parse().unwrap();
            (rotation, distance)
        })
        .collect()
}

fn star1(input: &[(Rotation, i32)]) -> i32 {
    let (mut x, mut y) = (0, 0);
    let (mut dx, mut dy) = (0, 1);
    for (rotation, distance) in input {
        match rotation {
            Rotation::Left => (dx, dy) = (-dy, dx),
            Rotation::Right => (dx, dy) = (dy, -dx),
        }
        x += distance * dx;
        y += distance * dy;
    }

    x.abs() + y.abs()
}

fn star2(input: &[(Rotation, i32)]) -> i32 {
    let (mut x, mut y) = (0i32, 0i32);
    let (mut dx, mut dy) = (0, 1);
    let mut visited = HashSet::new();
    visited.insert((x, y));

    for (rotation, distance) in input {
        match rotation {
            Rotation::Left => (dx, dy) = (-dy, dx),
            Rotation::Right => (dx, dy) = (dy, -dx),
        }
        for _ in 0..*distance {
            x += dx;
            y += dy;
            if !visited.insert((x, y)) {
                return x.abs() + y.abs();
            }
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
