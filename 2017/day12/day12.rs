#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::{HashMap, HashSet};

struct PipeGraph {
    adjacency: HashMap<i32, Vec<i32>>,
}

fn parse_input(input: &str) -> PipeGraph {
    fn parse_line(line: &str) -> (i32, Vec<i32>) {
        let (node, adjacency) = line.trim().split_once(" <-> ").unwrap();
        let adjacency = adjacency.split(", ").map(|i| i.parse().unwrap()).collect();
        (node.parse().unwrap(), adjacency)
    }

    PipeGraph {
        adjacency: input.lines().map(parse_line).collect(),
    }
}

fn star1(input: &PipeGraph) -> usize {
    let mut frontier = vec![0];
    let mut explored = HashSet::new();

    while let Some(curr_node) = frontier.pop() {
        if explored.insert(curr_node) {
            frontier.extend_from_slice(&input.adjacency[&curr_node])
        }
    }

    explored.len()
}

fn star2(input: &PipeGraph) -> usize {
    let mut nodes_to_check = input.adjacency.keys().copied().collect::<Vec<_>>();
    let mut explored = HashSet::new();
    let mut groups = 0;

    while let Some(to_check) = nodes_to_check.pop() {
        if explored.contains(&to_check) {
            continue;
        }

        groups += 1;
        let mut frontier = vec![to_check];
        while let Some(curr_node) = frontier.pop() {
            if explored.insert(curr_node) {
                frontier.extend_from_slice(&input.adjacency[&curr_node])
            }
        }
    }

    groups
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
