#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
};

#[derive(Debug, Clone)]
struct RewriteRule {
    source: String,
    target: String,
}

fn parse_input(input: String) -> (Vec<RewriteRule>, String) {
    let lines: Vec<_> = input.lines().collect();

    let rules = lines[..lines.len() - 2]
        .iter()
        .map(|rule| {
            let (source, target) = rule.split_once(" => ").unwrap();
            RewriteRule {
                source: source.to_owned(),
                target: target.to_owned(),
            }
        })
        .collect();

    let target_molecule = lines.last().unwrap().to_string();

    (rules, target_molecule)
}

fn step(rules: &[RewriteRule], curr_molecule: &str) -> Vec<String> {
    let mut next = HashSet::new();

    for rule in rules {
        for (idx, _) in curr_molecule.match_indices(&rule.source) {
            let mut new_molecule = curr_molecule.to_owned();
            new_molecule.replace_range(idx..idx + rule.source.len(), &rule.target);
            next.insert(new_molecule);
        }
    }

    next.into_iter().collect()
}

fn star1(input: &(Vec<RewriteRule>, String)) -> usize {
    let (rules, target_molecule) = input;

    step(rules, &target_molecule).len()
}

fn star2(input: &(Vec<RewriteRule>, String)) -> usize {
    let (rules, target_molecule) = input;

    let inverted_rules: Vec<_> = rules
        .iter()
        .map(|RewriteRule { source, target }| RewriteRule {
            source: target.to_owned(),
            target: source.to_owned(),
        })
        .collect();

    // Do a greedy best-first search, assuming there's only one way to generate
    // the target molecule
    let mut heap = BinaryHeap::new();
    let mut visited = HashSet::new();

    heap.push((Reverse(target_molecule.len()), 0, target_molecule.clone()));
    visited.insert(target_molecule.clone());

    while let Some((_, depth, curr)) = heap.pop() {
        if &curr == "e" {
            return depth;
        }

        for next in step(&inverted_rules, &curr) {
            if visited.insert(next.clone()) {
                heap.push((Reverse(next.len()), depth + 1, next));
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
