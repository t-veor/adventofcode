#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::{HashMap, HashSet};

const ROCKS: [&[(i32, i32)]; 5] = [
    &[(0, 0), (1, 0), (2, 0), (3, 0)],
    &[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    &[(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
    &[(0, 0), (0, 1), (0, 2), (0, 3)],
    &[(0, 0), (1, 0), (0, 1), (1, 1)],
];

#[derive(Debug, Default)]
struct Well {
    grid: HashSet<(i32, i32)>,
    highest: i32,
}

impl Well {
    fn can_fit(&self, rock: &[(i32, i32)], pos: (i32, i32)) -> bool {
        rock.iter().all(|piece| {
            let new_pos = (piece.0 + pos.0, piece.1 + pos.1);
            (0..7).contains(&new_pos.0) && new_pos.1 > 0 && !self.grid.contains(&new_pos)
        })
    }

    fn insert_rock(&mut self, rock: &[(i32, i32)], pos: (i32, i32)) {
        for piece in rock {
            let new_pos = (piece.0 + pos.0, piece.1 + pos.1);
            self.grid.insert(new_pos);

            if new_pos.1 > self.highest {
                self.highest = new_pos.1;
            }
        }
    }

    fn highest(&self) -> i32 {
        self.highest
    }

    #[allow(unused)]
    fn dbg_print(&self, new_rock: &[(i32, i32)], new_rock_pos: (i32, i32)) {
        let new_rocks: HashSet<_> = new_rock
            .iter()
            .map(|piece| (piece.0 + new_rock_pos.0, piece.1 + new_rock_pos.1))
            .collect();

        let highest = new_rocks
            .iter()
            .map(|(_, y)| *y)
            .max()
            .unwrap_or(self.highest);

        for y in (1..=highest).rev() {
            print!("|");
            for x in 0..7 {
                if self.grid.contains(&(x, y)) {
                    print!("#")
                } else if new_rocks.contains(&(x, y)) {
                    print!("@")
                } else {
                    print!(".")
                }
            }
            println!("|");
        }
        println!("+-------+");
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Left,
    Right,
}

#[derive(Debug, Default)]
struct RockFallState {
    well: Well,
    rock_idx: usize,
    wind_idx: usize,
}

impl RockFallState {
    fn simulate(&mut self, winds: &[Direction]) -> (i32, i32) {
        let rock = ROCKS[self.rock_idx];
        self.rock_idx += 1;
        if self.rock_idx >= ROCKS.len() {
            self.rock_idx = 0;
        }

        let mut pos = (2, self.well.highest() + 4);

        loop {
            let wind = winds[self.wind_idx];
            self.wind_idx += 1;
            if self.wind_idx >= winds.len() {
                self.wind_idx = 0;
            }

            let tentative_pos = match wind {
                Direction::Left => (pos.0 - 1, pos.1),
                Direction::Right => (pos.0 + 1, pos.1),
            };
            if self.well.can_fit(rock, tentative_pos) {
                pos = tentative_pos;
            }

            let fall_pos = (pos.0, pos.1 - 1);
            if self.well.can_fit(rock, fall_pos) {
                pos = fall_pos;
            } else {
                self.well.insert_rock(rock, pos);
                return pos;
            }
        }
    }
}

fn parse_input(input: String) -> Vec<Direction> {
    input
        .as_bytes()
        .iter()
        .filter_map(|c| match c {
            b'<' => Some(Direction::Left),
            b'>' => Some(Direction::Right),
            _ => None,
        })
        .collect()
}

fn star1(input: &[Direction]) -> i32 {
    let mut state = RockFallState::default();
    for _ in 0..2022 {
        state.simulate(input);
    }
    state.well.highest()
}

fn star2(input: &[Direction]) -> u64 {
    const REQUIRED: u64 = 1000000000000;

    let mut state = RockFallState::default();
    let mut pairs = HashMap::new();

    let mut remaining_rocks_required = REQUIRED;
    let mut factored_out_height = 0;

    let mut cycle_found = false;
    let mut cycle_lengths = HashMap::new();

    for rock_idx in 0.. {
        state.simulate(input);
        remaining_rocks_required -= 1;

        // This is not bulletproof cycle detection, but it's more than good
        // enough for almost all inputs on this problem. What I do is record any
        // candidate cycles where the (rock_idx, wind_idx, height_delta)
        // repeats, and if the repeat length occurs consistently enough (here
        // we've said 20 times) then we just assume that's a regular repeating
        // cycle. It would not be impossible to construct an input that breaks
        // this but it would be quite difficult for it to happen by accident.

        // I'm unhappy with this as it seems like bulletproof cycle detection
        // would require you to cache the exact boundary of the surface of the
        // well, but this surface could become arbitrarily large.
        if !cycle_found {
            let pair = (state.rock_idx, state.wind_idx);
            if let Some((old, old_height)) = pairs.insert(pair, (rock_idx, state.well.highest())) {
                let candidate_cycle = (rock_idx - old, state.well.highest() - old_height);
                let cycle_freq = cycle_lengths.entry(candidate_cycle).or_insert(0);
                *cycle_freq += 1;

                if *cycle_freq > 20 {
                    let cycle_length = candidate_cycle.0;
                    let height_gain = candidate_cycle.1 as u64;

                    let cycles = remaining_rocks_required / cycle_length;
                    remaining_rocks_required %= cycle_length;
                    factored_out_height = height_gain * cycles;
                    cycle_found = true;
                }
            }
        }

        if remaining_rocks_required == 0 {
            break;
        }
    }

    factored_out_height + state.well.highest() as u64
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
