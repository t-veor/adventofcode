#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn delta(&self) -> (i32, i32) {
        match self {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }
}

fn parse_input(input: String) -> Vec<Direction> {
    input
        .as_bytes()
        .iter()
        .filter_map(|c| {
            Some(match c {
                b'^' => Direction::Up,
                b'v' => Direction::Down,
                b'<' => Direction::Left,
                b'>' => Direction::Right,
                _ => return None,
            })
        })
        .collect()
}

fn get_visited_positions(
    start_pos: (i32, i32),
    dirs: impl Iterator<Item = Direction>,
) -> HashSet<(i32, i32)> {
    let mut visited = HashSet::new();
    let (mut x, mut y) = start_pos;
    visited.insert((x, y));

    for dir in dirs {
        let (dx, dy) = dir.delta();
        x += dx;
        y += dy;

        visited.insert((x, y));
    }

    visited
}

fn star1(input: &[Direction]) -> usize {
    get_visited_positions((0, 0), input.iter().copied()).len()
}

fn star2(input: &[Direction]) -> usize {
    let santa = get_visited_positions((0, 0), input.iter().step_by(2).copied());
    let robo_santa = get_visited_positions((0, 0), input.iter().skip(1).step_by(2).copied());
    santa.union(&robo_santa).count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
