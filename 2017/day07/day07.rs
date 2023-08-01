#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

#[derive(Debug)]
struct Program {
    weight: i32,
    children: Vec<String>,
}

#[derive(Debug)]
struct Tower {
    programs: HashMap<String, Program>,
    parents: HashMap<String, String>,
}

fn parse_input(input: &str) -> Tower {
    let mut programs = HashMap::new();
    let mut parents = HashMap::new();

    let p: &[_] = &['(', ')'];
    for line in input.lines() {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        let name = split[0];
        let weight = split[1].trim_matches(p);

        let mut program = Program {
            weight: weight.parse().unwrap(),
            children: Vec::new(),
        };

        if split.len() > 3 {
            for child in &split[3..] {
                let child = child.trim_matches(',');
                program.children.push(child.to_owned());
                parents.insert(child.to_owned(), name.to_owned());
            }
        }

        programs.insert(name.to_owned(), program);
    }

    Tower { programs, parents }
}

fn star1(input: &Tower) -> &str {
    for name in input.programs.keys() {
        if !input.parents.contains_key(name) {
            return name;
        }
    }
    unreachable!()
}

fn star2(input: &Tower) -> i32 {
    fn recursive_calc_weight(
        tower: &Tower,
        current: &str,
        subtower_weights: &mut HashMap<String, i32>,
    ) -> i32 {
        if let Some(weight) = subtower_weights.get(current) {
            return *weight;
        }

        let weight = {
            let program = &tower.programs[current];
            let mut weight = program.weight;
            for child in program.children.iter() {
                weight += recursive_calc_weight(tower, child, subtower_weights);
            }
            weight
        };

        subtower_weights.insert(current.to_owned(), weight);
        weight
    }

    let mut subtower_weights = HashMap::new();
    let root = star1(input);
    recursive_calc_weight(input, root, &mut subtower_weights);

    let mut curr = root;
    let mut expected_weight: Option<i32> = None;
    'outer: loop {
        let program = &input.programs[curr];
        let child_weights: Vec<_> = program
            .children
            .iter()
            .map(|child| subtower_weights[child])
            .collect();

        if child_weights.is_empty() || child_weights.iter().all(|&x| x == child_weights[0]) {
            let expected_weight = expected_weight.unwrap();
            let children_weight = child_weights.iter().sum::<i32>();
            return expected_weight - children_weight;
        }

        if child_weights.len() <= 2 {
            unreachable!("Unable to determine which subtower is unbalanced!");
        }

        let common_weight = {
            if child_weights[0] == child_weights[1] {
                child_weights[0]
            } else if child_weights[1] == child_weights[2] {
                child_weights[1]
            } else {
                child_weights[0]
            }
        };
        expected_weight = Some(common_weight);

        for i in 0..child_weights.len() {
            if child_weights[i] != common_weight {
                curr = &program.children[i];
                continue 'outer;
            }
        }

        unreachable!()
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
