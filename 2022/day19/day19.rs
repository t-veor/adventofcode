#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! regex = "1"
//! ```

// Another one about pruning the search space. I feel like there shouldn't
// actually be as many states as my program is exploring, but I'm done
// optimising for today. The program runs in about 1.3 seconds anyway

use std::{
    collections::HashMap,
    ops::{Add, Sub},
};

use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Vec4([u32; 4]);

impl Vec4 {
    fn leq(&self, other: &Self) -> bool {
        let u = &self.0;
        let v = &other.0;
        u[0] <= v[0] && u[1] <= v[1] && u[2] <= v[2] && u[3] <= v[3]
    }

    fn any_greater(&self, other: &Self) -> bool {
        let u = &self.0;
        let v = &other.0;
        u[0] > v[0] || u[1] > v[1] || u[2] > v[2] || u[3] > v[3]
    }

    fn max(&self, other: &Self) -> Self {
        let u = &self.0;
        let v = &other.0;
        Self([
            u[0].max(v[0]),
            u[1].max(v[1]),
            u[2].max(v[2]),
            u[3].max(v[3]),
        ])
    }
}

impl Add for Vec4 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let u = self.0;
        let v = rhs.0;
        Self([u[0] + v[0], u[1] + v[1], u[2] + v[2], u[3] + v[3]])
    }
}

impl Sub for Vec4 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let u = self.0;
        let v = rhs.0;
        Self([u[0] - v[0], u[1] - v[1], u[2] - v[2], u[3] - v[3]])
    }
}

#[derive(Debug, Clone)]
struct Blueprint {
    id: u32,
    costs: [Vec4; 4],
    max_costs: Vec4,
}

fn parse_input(input: String) -> Vec<Blueprint> {
    let re = Regex::new(
        r"Blueprint (\d+):\s*Each ore robot costs (\d+) ore.\s*Each clay robot costs (\d+) ore.\s*Each obsidian robot costs (\d+) ore and (\d+) clay.\s*Each geode robot costs (\d+) ore and (\d+) obsidian.",
    ).unwrap();

    re.captures_iter(&input)
        .map(|cap| {
            let costs = [
                Vec4([cap[2].parse().unwrap(), 0, 0, 0]),
                Vec4([cap[3].parse().unwrap(), 0, 0, 0]),
                Vec4([cap[4].parse().unwrap(), cap[5].parse().unwrap(), 0, 0]),
                Vec4([cap[6].parse().unwrap(), 0, cap[7].parse().unwrap(), 0]),
            ];
            let max_costs = costs.iter().fold(Vec4([0, 0, 0, 0]), |a, b| a.max(b));

            Blueprint {
                id: cap[1].parse().unwrap(),
                costs: costs,
                max_costs,
            }
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MoveState {
    remaining_time: u32,
    robots: Vec4,
    resources: Vec4,
    skipped_robots: [bool; 4],
}

impl MoveState {
    fn new(initial_time: u32) -> Self {
        Self {
            remaining_time: initial_time,
            robots: Vec4([1, 0, 0, 0]),
            resources: Vec4([0, 0, 0, 0]),
            skipped_robots: [false; 4],
        }
    }

    fn is_end(&self) -> bool {
        self.remaining_time == 0
    }

    fn cap_resources(remaining_time: u32, mut resources: Vec4, blueprint: &Blueprint) -> Vec4 {
        let resource_limit = blueprint.max_costs.0.map(|x| x * remaining_time);
        for i in 0..3 {
            resources.0[i] = resources.0[i].min(resource_limit[i]);
        }
        resources
    }

    fn expand(&self, blueprint: &Blueprint) -> Vec<Self> {
        if self.remaining_time == 0 {
            return vec![];
        }

        let mut moves = Vec::with_capacity(5);

        // Try pushing do-nothing move
        if blueprint.costs.iter().enumerate().any(|(i, cost)| {
            !self.skipped_robots[i]
                && cost.any_greater(&self.resources)
                && (0..4).all(|j| cost.0[j] == 0 || self.robots.0[j] > 0)
        }) {
            let skipped_robots = (0..4)
                .map(|i| self.skipped_robots[i] || blueprint.costs[i].leq(&self.resources))
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            moves.push(MoveState {
                remaining_time: self.remaining_time - 1,
                robots: self.robots,
                resources: Self::cap_resources(
                    self.remaining_time - 1,
                    self.resources + self.robots,
                    blueprint,
                ),
                skipped_robots,
            });
        }

        // Try building one of every robot
        for i in 0..4 {
            if self.skipped_robots[i] {
                // Pruning - if we intentionally skipped this robot, don't
                // bother generating a move that builds it until we build
                // something else
                continue;
            }

            if i != 3
                && self.resources.0[i] + self.remaining_time * self.robots.0[i]
                    >= self.remaining_time * blueprint.max_costs.0[i]
            {
                // Pruning - we have more of this resource than we could ever
                // use in the remaining time
                continue;
            }

            if blueprint.costs[i].leq(&self.resources) {
                let new_resources = Self::cap_resources(
                    self.remaining_time - 1,
                    self.resources - blueprint.costs[i] + self.robots,
                    blueprint,
                );
                let mut new_robots = self.robots;
                new_robots.0[i] += 1;

                moves.push(MoveState {
                    remaining_time: self.remaining_time - 1,
                    robots: new_robots,
                    resources: new_resources,
                    skipped_robots: [false; 4],
                })
            }
        }

        moves
    }
}

fn find_max_geodes(initial_time: u32, blueprint: &Blueprint) -> u32 {
    fn recurse(
        state: MoveState,
        blueprint: &Blueprint,
        cache: &mut HashMap<MoveState, u32>,
        inferior_state_pruning_cache: &mut HashMap<(u32, Vec4), Vec4>,
    ) -> u32 {
        if state.is_end() {
            return state.resources.0[3];
        }

        if let Some(result) = cache.get(&state) {
            return *result;
        }

        let mut result = 0;
        let children = state.expand(blueprint);
        for child_state in children {
            result = result.max(recurse(
                child_state,
                blueprint,
                cache,
                inferior_state_pruning_cache,
            ));
        }

        cache.insert(state, result);

        result
    }

    let mut cache = HashMap::new();
    let geode_count = recurse(
        MoveState::new(initial_time),
        blueprint,
        &mut cache,
        &mut HashMap::new(),
    );
    geode_count
}

fn star1(input: &[Blueprint]) -> u32 {
    input
        .iter()
        .map(|blueprint| find_max_geodes(24, blueprint) * blueprint.id)
        .sum()
}

fn star2(input: &[Blueprint]) -> u32 {
    input[..3.min(input.len())]
        .iter()
        .map(|blueprint| find_max_geodes(32, blueprint))
        .product()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
