#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{ops::Add, str::FromStr};

#[derive(Debug, Default, Clone, Copy)]
struct HexCoords {
    q: i32,
    r: i32,
}

impl HexCoords {
    fn new(q: i32, r: i32) -> Self {
        Self { q, r }
    }

    fn s(self) -> i32 {
        -self.q - self.r
    }

    fn distance_to_origin(self) -> i32 {
        (self.q.abs() + self.r.abs() + self.s().abs()) / 2
    }
}

impl Add<HexCoords> for HexCoords {
    type Output = Self;

    fn add(self, rhs: HexCoords) -> Self::Output {
        Self {
            q: self.q + rhs.q,
            r: self.r + rhs.r,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum HexDirection {
    North,
    NorthEast,
    SouthEast,
    South,
    SouthWest,
    NorthWest,
}

impl HexDirection {
    fn to_offset(self) -> HexCoords {
        match self {
            HexDirection::North => HexCoords::new(0, -1),
            HexDirection::NorthEast => HexCoords::new(1, -1),
            HexDirection::SouthEast => HexCoords::new(1, 0),
            HexDirection::South => HexCoords::new(0, 1),
            HexDirection::SouthWest => HexCoords::new(-1, 1),
            HexDirection::NorthWest => HexCoords::new(-1, 0),
        }
    }
}

impl FromStr for HexDirection {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "n" => Self::North,
            "ne" => Self::NorthEast,
            "nw" => Self::NorthWest,
            "s" => Self::South,
            "se" => Self::SouthEast,
            "sw" => Self::SouthWest,
            _ => return Err(()),
        })
    }
}

fn parse_input(input: &str) -> Vec<HexDirection> {
    input
        .trim()
        .split(',')
        .map(|i| i.parse().unwrap())
        .collect()
}

fn star1(input: &[HexDirection]) -> i32 {
    let mut position = HexCoords::default();
    for dir in input {
        position = position + dir.to_offset();
    }
    position.distance_to_origin()
}

fn star2(input: &[HexDirection]) -> i32 {
    let mut position = HexCoords::default();
    let mut furthest = 0;
    for dir in input {
        position = position + dir.to_offset();
        let dist = position.distance_to_origin();
        if dist > furthest {
            furthest = dist;
        }
    }
    furthest
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
