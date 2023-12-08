use std::collections::HashMap;

use itertools::Itertools;
use utils::{
    discrete_math::{chinese_remainder_theorem_non_coprime, lcm},
    read_input_file,
};

#[derive(Debug, Clone, Copy)]
enum Direction {
    Left,
    Right,
}

#[derive(Debug)]
struct Node {
    left: String,
    right: String,
}

#[derive(Debug)]
struct Network(HashMap<String, Node>);

fn parse_input(input: &str) -> (Vec<Direction>, Network) {
    let (directions, nodes) = input.split_once("\n\n").unwrap();

    let directions = directions
        .chars()
        .map(|char| match char {
            'L' => Direction::Left,
            'R' => Direction::Right,
            _ => panic!("Unknown direction {char}"),
        })
        .collect();

    let nodes = nodes
        .lines()
        .map(|line| {
            let (name, children) = line.split_once(" = ").unwrap();
            let (left, right) = children.split_once(", ").unwrap();
            let left = left.strip_prefix('(').unwrap();
            let right = right.strip_suffix(')').unwrap();

            (
                name.to_string(),
                Node {
                    left: left.to_string(),
                    right: right.to_string(),
                },
            )
        })
        .collect();

    (directions, Network(nodes))
}

fn star1(directions: &[Direction], network: &Network) -> usize {
    let mut current = "AAA";
    let mut steps = 0usize;

    while current != "ZZZ" {
        let node = &network.0[current];

        current = match directions[steps % directions.len()] {
            Direction::Left => &node.left,
            Direction::Right => &node.right,
        };

        steps += 1;
    }

    steps
}

#[derive(Debug)]
struct CycleInfo {
    start: usize,
    length: usize,
    destination_times_prior_to_cycle: Vec<usize>,
    destination_times_in_cycle: Vec<usize>,
}

fn detect_cycle(directions: &[Direction], network: &Network, starting_node: &str) -> CycleInfo {
    let mut seen = HashMap::new();
    let mut current = starting_node;
    let mut steps = 0usize;

    let mut times_on_destination = Vec::new();

    loop {
        let direction_idx = steps % directions.len();

        if let Some(cycle_start) = seen.insert((current.to_string(), direction_idx), steps) {
            return CycleInfo {
                start: cycle_start,
                length: steps - cycle_start,
                destination_times_prior_to_cycle: times_on_destination
                    .iter()
                    .copied()
                    .filter(|&i| i < cycle_start)
                    .collect(),
                destination_times_in_cycle: times_on_destination
                    .iter()
                    .copied()
                    .filter_map(|i| i.checked_sub(cycle_start))
                    .collect(),
            };
        }

        if current.ends_with('Z') {
            times_on_destination.push(steps);
        }

        let node = &network.0[current];
        current = match directions[direction_idx] {
            Direction::Left => &node.left,
            Direction::Right => &node.right,
        };

        steps += 1;
    }
}

fn star2(directions: &[Direction], network: &Network) -> usize {
    let starting_nodes: Vec<&str> = network
        .0
        .keys()
        .filter(|key| key.ends_with('A'))
        .map(|key| &key[..])
        .collect();

    let cycle_infos: Vec<_> = starting_nodes
        .iter()
        .map(|node| detect_cycle(directions, network, node))
        .collect();

    // This is a little annoying to check, but technically there could be a time
    // where all the ghosts are on a destination node before all of the cycles
    // have started. This doesn't seem to be the case for the actual input
    // though.
    let max_time_before_cycle = cycle_infos.iter().map(|info| info.start).max().unwrap_or(0);
    for steps in 0..max_time_before_cycle {
        if cycle_infos.iter().all(|cycle| {
            if steps < cycle.start {
                cycle.destination_times_prior_to_cycle.contains(&steps)
            } else {
                let steps_in_cycle = (steps - cycle.start) % cycle.length;
                cycle.destination_times_in_cycle.contains(&steps_in_cycle)
            }
        }) {
            return steps;
        }
    }

    // Otherwise, we end up with a system of congruences. We want to find steps
    // such that for each cycle:
    //   steps - cycle.start == d (mod cycle.length)
    // where d is one of the elements in cycle.destination_times_in_cycle.

    let bases: Vec<_> = cycle_infos.iter().map(|info| info.length as i64).collect();

    let possible_residue_combos = cycle_infos
        .iter()
        .map(|info| {
            info.destination_times_in_cycle
                .iter()
                .map(|d| ((info.start + d) % info.length) as i64)
        })
        .multi_cartesian_product();

    let lcm_all_cycle_lengths = bases.iter().copied().fold(1, lcm);

    let mut steps = possible_residue_combos
        .filter_map(|residues| chinese_remainder_theorem_non_coprime(&residues, &bases))
        .min()
        .expect("No valid steps found?");

    while steps < max_time_before_cycle as i64 {
        steps += lcm_all_cycle_lengths;
    }

    steps as usize
}

fn main() {
    let input = read_input_file!();
    let (directions, network) = parse_input(&input);

    println!("{}", star1(&directions, &network));
    println!("{}", star2(&directions, &network));
}
