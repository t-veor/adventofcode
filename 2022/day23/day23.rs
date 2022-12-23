#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::{HashMap, HashSet};

const DIRECTIONS: [(i32, i32); 4] = [(0, -1), (0, 1), (-1, 0), (1, 0)];

fn parse_input(input: String) -> HashSet<(i32, i32)> {
    let mut elves = HashSet::new();

    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.as_bytes().iter().enumerate() {
            if *c == b'#' {
                elves.insert((x as i32, y as i32));
            }
        }
    }

    elves
}

fn elves_in_direction(elves: &HashSet<(i32, i32)>, pos: (i32, i32)) -> [bool; 4] {
    // north, south, west, east
    let mut adjacency = [false; 4];

    let (x, y) = pos;
    for z in x - 1..=x + 1 {
        for w in y - 1..=y + 1 {
            if (z, w) == pos {
                continue;
            }

            if elves.contains(&(z, w)) {
                if w < y {
                    adjacency[0] = true;
                } else if w > y {
                    adjacency[1] = true;
                }
                if z < x {
                    adjacency[2] = true;
                } else if z > x {
                    adjacency[3] = true;
                }
            }
        }
    }

    adjacency
}

fn step(elves: &mut HashSet<(i32, i32)>, curr_iter: usize) -> bool {
    let mut proposals = HashMap::new();
    let mut proposed_moves = Vec::new();

    for (x, y) in elves.iter().copied() {
        let adjacency = elves_in_direction(elves, (x, y));
        if adjacency.iter().all(|x| !*x) {
            continue;
        }

        for d in 0..4 {
            let d = (curr_iter + d) % 4;

            let (dx, dy) = DIRECTIONS[d];
            if !adjacency[d] {
                proposed_moves.push(((x, y), (x + dx, y + dy)));
                *proposals.entry((x + dx, y + dy)).or_insert(0) += 1;
                break;
            }
        }
    }

    let no_more_moves = proposed_moves.is_empty();

    for (from, to) in proposed_moves {
        if proposals.get(&to).map(|x| *x < 2).unwrap_or(true) {
            elves.remove(&from);
            elves.insert(to);
        }
    }

    no_more_moves
}

#[allow(unused)]
fn debug_print(elves: &HashSet<(i32, i32)>) {
    let min_x = elves.iter().map(|(x, _)| *x).min().unwrap();
    let min_y = elves.iter().map(|(_, y)| *y).min().unwrap();
    let max_x = elves.iter().map(|(x, _)| *x).max().unwrap();
    let max_y = elves.iter().map(|(_, y)| *y).max().unwrap();

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            if elves.contains(&(x, y)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!()
    }
}

fn score(elves: &HashSet<(i32, i32)>) -> i32 {
    let min_x = elves.iter().map(|(x, _)| *x).min().unwrap();
    let min_y = elves.iter().map(|(_, y)| *y).min().unwrap();
    let max_x = elves.iter().map(|(x, _)| *x).max().unwrap();
    let max_y = elves.iter().map(|(_, y)| *y).max().unwrap();

    let width = max_x - min_x + 1;
    let height = max_y - min_y + 1;
    width * height - elves.len() as i32
}

fn star1(input: &HashSet<(i32, i32)>) -> i32 {
    let mut elves = input.clone();

    for i in 0..10 {
        step(&mut elves, i);
    }

    score(&elves)
}

fn star2(input: &HashSet<(i32, i32)>) -> usize {
    let mut elves = input.clone();

    for i in 0.. {
        if step(&mut elves, i) {
            return i + 1;
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
