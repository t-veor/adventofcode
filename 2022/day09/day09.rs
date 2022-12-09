#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn delta(&self) -> (i32, i32) {
        match self {
            Self::Up => (0, 1),
            Self::Down => (0, -1),
            Self::Left => (-1, 0),
            Self::Right => (1, 0),
        }
    }
}

fn parse_input(input: String) -> Vec<(Direction, usize)> {
    input
        .lines()
        .map(|line| {
            let (dir, steps) = line.split_once(' ').unwrap();
            let dir = match dir {
                "U" => Direction::Up,
                "D" => Direction::Down,
                "L" => Direction::Left,
                "R" => Direction::Right,
                _ => unreachable!(),
            };
            let steps = steps.parse().unwrap();
            (dir, steps)
        })
        .collect()
}

struct Rope {
    knots: Vec<(i32, i32)>,
}

impl Rope {
    fn new(length: usize) -> Self {
        Self {
            knots: vec![(0, 0); length],
        }
    }

    fn step(&mut self, dir: Direction) {
        let delta = dir.delta();
        self.knots[0].0 += delta.0;
        self.knots[0].1 += delta.1;

        for i in 1..self.knots.len() {
            let split = self.knots.split_at_mut(i);
            let (prev, curr) = (split.0.last_mut().unwrap(), split.1.first_mut().unwrap());

            let chebyshev_distance = (prev.0 - curr.0).abs().max((prev.1 - curr.1).abs());
            if chebyshev_distance > 1 {
                curr.0 += (prev.0 - curr.0).signum();
                curr.1 += (prev.1 - curr.1).signum();
            }
        }
    }

    fn tail_pos(&self) -> (i32, i32) {
        *self.knots.last().unwrap()
    }
}

fn simulate_rope(length: usize, input: &[(Direction, usize)]) -> usize {
    let mut visited = HashSet::new();
    let mut rope = Rope::new(length);

    for (direction, steps) in input {
        for _ in 0..*steps {
            rope.step(*direction);
            visited.insert(rope.tail_pos());
        }
    }

    visited.len()
}

fn star1(input: &[(Direction, usize)]) -> usize {
    simulate_rope(2, input)
}

fn star2(input: &[(Direction, usize)]) -> usize {
    simulate_rope(10, input)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
