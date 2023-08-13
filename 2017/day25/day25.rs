#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! regex = "1"
//! ```

use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum Direction {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct Transition {
    write: bool,
    direction: Direction,
    next_state: usize,
}

#[derive(Debug, Clone)]
struct StateTransitionTable(Vec<[Transition; 2]>);

#[derive(Debug, Clone, Default)]
struct Tape(Vec<bool>, Vec<bool>);

impl Tape {
    fn extend_and_get_mut(v: &mut Vec<bool>, idx: usize) -> &mut bool {
        while v.len() <= idx {
            v.push(false);
        }
        &mut v[idx]
    }

    fn get_or_insert(&mut self, idx: isize) -> &mut bool {
        if idx >= 0 {
            Self::extend_and_get_mut(&mut self.1, idx as usize)
        } else {
            Self::extend_and_get_mut(&mut self.0, (!idx) as usize)
        }
    }

    fn count_ones(&self) -> usize {
        self.0.iter().map(|&x| x as usize).sum::<usize>()
            + self.1.iter().map(|&x| x as usize).sum::<usize>()
    }
}

#[derive(Debug, Clone)]
struct TuringMachine {
    table: StateTransitionTable,
    position: isize,
    tape: Tape,
    state: usize,
}

impl TuringMachine {
    fn new(table: StateTransitionTable, initial_state: usize) -> Self {
        Self {
            table,
            position: 0,
            tape: Tape::default(),
            state: initial_state,
        }
    }

    fn step(&mut self) {
        let value = self.tape.get_or_insert(self.position);
        let transition = self.table.0[self.state][(*value) as usize];

        *value = transition.write;
        self.position += match transition.direction {
            Direction::Left => -1,
            Direction::Right => 1,
        };
        self.state = transition.next_state;
    }
}

fn parse_input(input: &str) -> (TuringMachine, usize) {
    let mut state_indices = HashMap::new();
    let mut transitions = Vec::new();

    let line_one = Regex::new(r"Begin in state (\w+)\.").unwrap();
    let line_two = Regex::new(r"Perform a diagnostic checksum after (\d+) steps\.").unwrap();

    let state_transition = Regex::new(
        r"In state (\w+):
  If the current value is 0:
    - Write the value (0|1)\.
    - Move one slot to the (left|right)\.
    - Continue with state (\w+)\.
  If the current value is 1:
    - Write the value (0|1)\.
    - Move one slot to the (left|right)\.
    - Continue with state (\w+)\.",
    )
    .unwrap();

    let start_state = &line_one.captures(input).unwrap()[1];
    let num_steps = &line_two.captures(input).unwrap()[1];

    for capture in state_transition.captures_iter(input) {
        let state_name = &capture[1];

        let zero_write = &capture[2] == "1";
        let zero_dir = &capture[3] == "right";
        let zero_next = capture[4].to_string();

        let one_write = &capture[5] == "1";
        let one_dir = &capture[6] == "right";
        let one_next = capture[7].to_string();

        let state_id = transitions.len();
        state_indices.insert(state_name.to_string(), state_id);

        transitions.push([
            (zero_write, zero_dir, zero_next),
            (one_write, one_dir, one_next),
        ]);
    }

    let transition_table = StateTransitionTable(
        transitions
            .into_iter()
            .map(|table| {
                table.map(|(write, dir, next)| Transition {
                    write,
                    direction: match dir {
                        true => Direction::Right,
                        false => Direction::Left,
                    },
                    next_state: state_indices[&next],
                })
            })
            .collect(),
    );

    let initial_state = state_indices[start_state];

    (
        TuringMachine::new(transition_table, initial_state),
        num_steps.parse().unwrap(),
    )
}

fn star1((machine, steps): &(TuringMachine, usize)) -> usize {
    let mut machine = machine.clone();
    for _ in 0..*steps {
        machine.step();
    }

    machine.tape.count_ones()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
}
