#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! md5 = "0.7"
//! ```

use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    const ALL: [Self; 4] = [
        Direction::Up,
        Direction::Down,
        Direction::Left,
        Direction::Right,
    ];

    fn delta(&self) -> (i32, i32) {
        match self {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }
}

fn get_doors(passcode: &str, moves: &[Direction]) -> [bool; 4] {
    let mut hash_input = String::with_capacity(passcode.len() + moves.len());
    hash_input.push_str(passcode);
    for dir in moves {
        hash_input.push(match dir {
            Direction::Up => 'U',
            Direction::Down => 'D',
            Direction::Left => 'L',
            Direction::Right => 'R',
        });
    }

    let digest = md5::compute(hash_input);
    let up = (digest.0[0] & 0xf0) >> 4;
    let down = digest.0[0] & 0x0f;
    let left = (digest.0[1] & 0xf0) >> 4;
    let right = digest.0[1] & 0x0f;

    [up, down, left, right].map(|x| (0xb..=0xf).contains(&x))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    position: (i32, i32),
    moves: Vec<Direction>,
}

impl State {
    fn initial() -> Self {
        Self {
            position: (0, 0),
            moves: vec![],
        }
    }

    fn is_goal(&self) -> bool {
        self.position == (3, 3)
    }

    fn expand(&self, passcode: &str) -> Vec<Self> {
        let mut children = Vec::with_capacity(4);
        let doors = get_doors(passcode, &self.moves);

        let (x, y) = self.position;
        for dir in Direction::ALL {
            let (dx, dy) = dir.delta();
            let (x, y) = (x + dx, y + dy);

            if (0..4).contains(&x) && (0..4).contains(&y) && doors[dir as usize] {
                let mut new_moves = Vec::with_capacity(self.moves.len() + 1);
                new_moves.extend_from_slice(&self.moves);
                new_moves.push(dir);
                children.push(Self {
                    position: (x, y),
                    moves: new_moves,
                })
            }
        }

        children
    }
}

fn bfs<T, I>(
    initial_state: T,
    is_goal: impl Fn(&T) -> bool,
    expand: impl Fn(&T) -> I,
    mut listener: impl FnMut(T) -> bool,
) where
    T: Eq + Hash + Clone,
    I: IntoIterator<Item = T>,
{
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

    visited.insert(initial_state.clone());
    queue.push_back(initial_state);

    while let Some(state) = queue.pop_front() {
        if is_goal(&state) {
            if listener(state) {
                return;
            } else {
                continue;
            }
        }

        for next_state in expand(&state) {
            if visited.insert(next_state.clone()) {
                queue.push_back(next_state);
            }
        }
    }
}

fn parse_input(input: &str) -> &str {
    input.trim()
}

fn star1(input: &str) -> String {
    let mut goal = None;
    bfs(
        State::initial(),
        |state| state.is_goal(),
        |state| state.expand(input),
        |state| {
            goal = Some(state);
            true
        },
    );

    let goal = goal.expect("No path to end");

    goal.moves
        .iter()
        .map(|dir| match dir {
            Direction::Up => 'U',
            Direction::Down => 'D',
            Direction::Left => 'L',
            Direction::Right => 'R',
        })
        .collect()
}

fn star2(input: &str) -> usize {
    let mut goal = None;
    bfs(
        State::initial(),
        |state| state.is_goal(),
        |state| state.expand(input),
        |state| {
            goal = Some(state);
            false
        },
    );

    let goal = goal.expect("No path to end");

    goal.moves.len()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
