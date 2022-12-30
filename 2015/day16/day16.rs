#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

#[derive(Debug)]
struct Properties(HashMap<String, i32>);

impl Properties {
    fn contains(&self, other: &Self) -> bool {
        for (key, value) in other.0.iter() {
            if self.0[key] != *value {
                return false;
            }
        }

        true
    }

    fn contains_part_2(&self, other: &Self) -> bool {
        for (key, value) in other.0.iter() {
            let fits = match key.as_ref() {
                "cats" | "trees" => *value > self.0[key],
                "pomeranians" | "goldfish" => *value < self.0[key],
                _ => *value == self.0[key],
            };

            if !fits {
                return false;
            }
        }

        true
    }
}

fn target_properties() -> Properties {
    const TARGET: &str = "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1";

    Properties(
        TARGET
            .lines()
            .map(|line| {
                let (key, value) = line.split_once(": ").unwrap();
                (key.to_owned(), value.parse().unwrap())
            })
            .collect(),
    )
}

#[derive(Debug)]
struct Sue {
    id: i32,
    properties: Properties,
}

fn parse_input(input: String) -> Vec<Sue> {
    fn extract(line: &str) -> Sue {
        let (name, properties) = line.split_once(": ").unwrap();
        let id = name.split_once(' ').unwrap().1.parse().unwrap();
        let properties = Properties(
            properties
                .split(", ")
                .map(|i| {
                    let (key, value) = i.split_once(": ").unwrap();
                    (key.to_owned(), value.parse().unwrap())
                })
                .collect(),
        );

        Sue { id, properties }
    }

    input.lines().map(extract).collect()
}

fn star1(input: &[Sue]) -> i32 {
    let target = target_properties();

    for sue in input {
        if target.contains(&sue.properties) {
            return sue.id;
        }
    }

    unreachable!()
}

fn star2(input: &[Sue]) -> i32 {
    let target = target_properties();

    for sue in input {
        if target.contains_part_2(&sue.properties) {
            return sue.id;
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
