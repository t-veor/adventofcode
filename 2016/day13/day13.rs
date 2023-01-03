#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! pathfinding = "4.2"
//! ```

use std::{collections::HashSet, hash::Hash};

use pathfinding::prelude::astar;

fn parse_input(input: &str) -> i32 {
    input.trim().parse().unwrap()
}

fn is_wall(seed: i32, pos: (i32, i32)) -> bool {
    let (x, y) = pos;
    let num = x * x + 3 * x + 2 * x * y + y + y * y + seed;
    num.count_ones() % 2 == 1
}

fn get_adjacent(seed: i32, pos: (i32, i32)) -> Vec<(i32, i32)> {
    let (x, y) = pos;
    let mut adjacent = Vec::with_capacity(4);

    if x > 0 && !is_wall(seed, (x - 1, y)) {
        adjacent.push((x - 1, y));
    }
    if !is_wall(seed, (x + 1, y)) {
        adjacent.push((x + 1, y));
    }

    if y > 0 && !is_wall(seed, (x, y - 1)) {
        adjacent.push((x, y - 1));
    }
    if !is_wall(seed, (x, y + 1)) {
        adjacent.push((x, y + 1));
    }

    adjacent
}

fn manhattan_distance(from: (i32, i32), to: (i32, i32)) -> i32 {
    (from.0 - to.0).abs() + (from.1 - to.1).abs()
}

fn star1(input: &i32) -> i32 {
    let seed = *input;
    let goal = (31, 39);
    let (_path, length) = astar(
        &(1, 1),
        |&pos| get_adjacent(seed, pos).into_iter().map(|x| (x, 1)),
        |&pos| manhattan_distance(pos, goal),
        |&pos| pos == goal,
    )
    .expect("no path found");

    length
}

// Annoyingly the pathfinding library doesn't have an algorithm implemented for
// (nodes reachable within n steps of a bfs), so I'll just implement it myself

fn bfs_reachable<T, IT>(initial_state: &T, expand: impl Fn(&T) -> IT, steps: u32) -> usize
where
    T: Eq + Hash + Clone,
    IT: IntoIterator<Item = T>,
{
    let mut seen = HashSet::new();

    let mut curr_queue = vec![initial_state.clone()];
    let mut next_queue = Vec::new();
    seen.insert(initial_state.clone());

    for _ in 0..steps {
        for state in curr_queue.drain(..) {
            for next_state in expand(&state) {
                if seen.insert(next_state.clone()) {
                    next_queue.push(next_state);
                }
            }
        }

        std::mem::swap(&mut curr_queue, &mut next_queue);
    }

    seen.len()
}

fn star2(input: &i32) -> usize {
    let seed = *input;
    bfs_reachable(&(1, 1), |&pos| get_adjacent(seed, pos), 50)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
