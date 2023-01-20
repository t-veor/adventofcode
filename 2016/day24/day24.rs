#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! itertools = "0.10"
//! ```

use itertools::Itertools;
use std::collections::{HashSet, VecDeque};

#[derive(Debug, Clone)]
struct DistanceMatrix {
    size: usize,
    matrix: Vec<u32>,
}

impl DistanceMatrix {
    #[allow(unused)]
    fn debug_print(&self) {
        for (i, d) in self.matrix.iter().enumerate() {
            print!("{}\t", d);
            if i % self.size == self.size - 1 {
                println!();
            }
        }
        println!();
    }
}

#[derive(Debug, Clone)]
struct Maze {
    width: usize,
    height: usize,
    walls: Vec<bool>,
    points_of_interest: Vec<usize>,
}

impl Maze {
    fn from_str(input: &str) -> Self {
        let height = input.lines().count();
        let width = input.lines().next().unwrap().len();
        let mut walls = vec![true; width * height];
        let mut points_of_interest = Vec::new();

        for (y, line) in input.lines().enumerate() {
            for (x, c) in line.as_bytes().iter().enumerate() {
                let idx = y * width + x;
                if *c != b'#' {
                    walls[idx] = false;
                    if c.is_ascii_digit() {
                        let point_of_interest = (*c - b'0') as usize;
                        while points_of_interest.len() <= point_of_interest {
                            points_of_interest.push(0);
                        }
                        points_of_interest[point_of_interest] = idx;
                    }
                }
            }
        }

        Self {
            width,
            height,
            walls,
            points_of_interest,
        }
    }

    fn to_coords(&self, idx: usize) -> (usize, usize) {
        let y = idx / self.width;
        let x = idx % self.width;
        (x, y)
    }

    fn bfs_to_all_points_of_interest(&self, start: usize) -> Vec<u32> {
        let mut poi_dists = vec![u32::MAX; self.points_of_interest.len()];

        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        visited.insert(start);
        queue.push_back((start, 0));

        while let Some((pos, depth)) = queue.pop_front() {
            if let Some(poi) = self.points_of_interest.iter().position(|&x| x == pos) {
                poi_dists[poi] = depth;
            }

            let mut neighbors = Vec::with_capacity(4);
            let (x, y) = self.to_coords(pos);
            if x > 0 && !self.walls[pos - 1] {
                neighbors.push(pos - 1);
            }
            if x < self.width - 1 && !self.walls[pos + 1] {
                neighbors.push(pos + 1);
            }
            if y > 0 && !self.walls[pos - self.width] {
                neighbors.push(pos - self.width);
            }
            if y < self.height - 1 && !self.walls[pos + self.width] {
                neighbors.push(pos + self.width);
            }

            for next_pos in neighbors {
                if visited.insert(next_pos) {
                    queue.push_back((next_pos, depth + 1));
                }
            }
        }

        poi_dists
    }

    fn points_of_interest_shortest_paths(&self) -> DistanceMatrix {
        let poi_count = self.points_of_interest.len();
        let mut matrix = vec![0; poi_count * poi_count];

        for (i, x) in self.points_of_interest.iter().enumerate() {
            matrix[i * poi_count..(i + 1) * poi_count]
                .copy_from_slice(&self.bfs_to_all_points_of_interest(*x));
        }

        DistanceMatrix {
            size: poi_count,
            matrix,
        }
    }
}

fn parse_input(input: &str) -> Maze {
    Maze::from_str(input)
}

fn star1(input: &Maze) -> u32 {
    let dists = input.points_of_interest_shortest_paths();

    (1..dists.size)
        .permutations(dists.size.saturating_sub(1))
        .map(|route| {
            let mut length = dists.matrix[route[0]];
            for pair in route.windows(2) {
                let i = pair[0];
                let j = pair[1];
                length += dists.matrix[i * dists.size + j];
            }
            length
        })
        .min()
        .unwrap()
}

fn star2(input: &Maze) -> u32 {
    let dists = input.points_of_interest_shortest_paths();

    (1..dists.size)
        .permutations(dists.size.saturating_sub(1))
        .map(|route| {
            let mut length = dists.matrix[route[0]];
            for pair in route.windows(2) {
                let i = pair[0];
                let j = pair[1];
                length += dists.matrix[i * dists.size + j];
            }
            length += dists.matrix[*route.last().unwrap() * dists.size];
            length
        })
        .min()
        .unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
