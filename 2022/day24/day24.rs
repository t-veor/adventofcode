#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! gcd = "2.2"
//! ```

use gcd::Gcd;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone)]
struct ValleyState {
    width: i32,
    height: i32,
    blizzards: HashMap<(i32, i32), Vec<Direction>>,
}

impl ValleyState {
    fn step(&self) -> Self {
        let mut blizzards = HashMap::<(i32, i32), Vec<Direction>>::new();

        for ((x, y), blizzards_on_this_square) in self.blizzards.iter() {
            for direction in blizzards_on_this_square {
                let (mut new_x, mut new_y) = match direction {
                    Direction::Up => (*x, y - 1),
                    Direction::Down => (*x, y + 1),
                    Direction::Left => (x - 1, *y),
                    Direction::Right => (x + 1, *y),
                };

                if new_x < 1 {
                    new_x = self.width - 2;
                } else if new_x > self.width - 2 {
                    new_x = 1;
                }
                if new_y < 1 {
                    new_y = self.height - 2;
                } else if new_y > self.height - 2 {
                    new_y = 1;
                }

                blizzards
                    .entry((new_x, new_y))
                    .or_default()
                    .push(*direction);
            }
        }

        Self {
            width: self.width,
            height: self.height,
            blizzards,
        }
    }

    #[allow(unused)]
    fn debug_print(&self, elves_position: Option<(i32, i32)>) {
        for y in 0..self.height {
            for x in 0..self.width {
                if elves_position == Some((x, y)) {
                    print!("E")
                } else if (x, y) == (1, 0) || (x, y) == (self.width - 2, self.height - 1) {
                    print!(".")
                } else if x < 1 || x > self.width - 2 || y < 1 || y > self.height - 2 {
                    print!("#")
                } else if let Some(dirs) = self.blizzards.get(&(x, y)) {
                    if dirs.len() == 1 {
                        print!(
                            "{}",
                            match dirs[0] {
                                Direction::Up => "^",
                                Direction::Down => "v",
                                Direction::Left => "<",
                                Direction::Right => ">",
                            }
                        )
                    } else {
                        print!("{}", dirs.len())
                    }
                } else {
                    print!(".")
                }
            }
            println!()
        }
    }
}

#[derive(Debug, Clone)]
struct Valley {
    width: i32,
    height: i32,
    period: usize,
    timesteps: Vec<ValleyState>,
}

impl Valley {
    fn new(initial_state: ValleyState) -> Self {
        // We actually know that the state must repeat, so we can reduce the
        // number of states we need to store.
        let valley_width = initial_state.width as usize - 2;
        let valley_height = initial_state.height as usize - 2;
        let period = valley_width * valley_height / (valley_width).gcd(valley_height);

        Self {
            width: initial_state.width,
            height: initial_state.height,
            period,
            timesteps: vec![initial_state],
        }
    }

    fn get_state_at(&mut self, timestep: usize) -> &ValleyState {
        let timestep = timestep % self.period;

        while self.timesteps.len() <= timestep {
            let next = self.timesteps.last().unwrap().step();
            self.timesteps.push(next)
        }

        &self.timesteps[timestep]
    }
}

fn parse_input(input: String) -> Valley {
    let mut blizzards = HashMap::<(i32, i32), Vec<Direction>>::new();

    let height = input.lines().count() as _;
    let width = input.lines().next().unwrap().len() as _;

    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.as_bytes().iter().enumerate() {
            let dir = match *c {
                b'>' => Direction::Right,
                b'<' => Direction::Left,
                b'^' => Direction::Up,
                b'v' => Direction::Down,
                _ => continue,
            };
            blizzards.insert((x as _, y as _), vec![dir]);
        }
    }

    Valley::new(ValleyState {
        width,
        height,
        blizzards,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct MoveState {
    timestep: usize,
    position: (i32, i32),
}

impl MoveState {
    fn initial_state() -> Self {
        Self {
            timestep: 0,
            position: (1, 0),
        }
    }

    fn is_goal(&self, valley: &Valley) -> bool {
        self.position == (valley.width - 2, valley.height - 1)
    }

    fn expand(&self, valley: &mut Valley) -> Vec<Self> {
        const DELTAS: [(i32, i32); 5] = [(1, 0), (-1, 0), (0, 1), (0, -1), (0, 0)];
        let mut moves = Vec::with_capacity(5);

        let timestep = self.timestep + 1;
        let (width, height) = (valley.width, valley.height);
        let valley_state = valley.get_state_at(timestep);
        for (dx, dy) in DELTAS.iter().copied() {
            let new_pos = (self.position.0 + dx, self.position.1 + dy);

            let is_not_wall = new_pos == (1, 0)
                || new_pos == (width - 2, height - 1)
                || (new_pos.0 >= 1
                    && new_pos.0 <= width - 2
                    && new_pos.1 >= 1
                    && new_pos.1 <= height - 2);

            if is_not_wall && !valley_state.blizzards.contains_key(&new_pos) {
                moves.push(MoveState {
                    timestep,
                    position: new_pos,
                });
            }
        }

        moves
    }
}

// It looks like the branching factor is not that big. Maybe a BFS will just
// solve it?

fn bfs<T>(initial_state: T, is_goal: impl Fn(&T) -> bool, mut expand: impl FnMut(&T) -> Vec<T>) -> T
where
    T: Eq + Clone + Hash,
{
    let mut queue = VecDeque::new();
    let mut explored = HashSet::new();

    queue.push_back(initial_state.clone());
    explored.insert(initial_state);

    while let Some(state) = queue.pop_front() {
        if is_goal(&state) {
            return state;
        }

        for new_state in expand(&state) {
            if explored.insert(new_state.clone()) {
                queue.push_back(new_state);
            }
        }
    }

    panic!("no path found")
}

fn star1(input: &Valley) -> usize {
    let mut valley = input.clone();

    bfs(
        MoveState::initial_state(),
        |state| state.is_goal(&input),
        |state| state.expand(&mut valley),
    )
    .timestep
}

fn star2(input: &Valley) -> usize {
    let mut valley = input.clone();

    let start_pos = (1, 0);
    let end_pos = (valley.width - 2, valley.height - 1);

    let state_0 = bfs(
        MoveState::initial_state(),
        |state| state.position == end_pos,
        |state| state.expand(&mut valley),
    );

    let state_1 = bfs(
        state_0,
        |state| state.position == start_pos,
        |state| state.expand(&mut valley),
    );

    let state_2 = bfs(
        state_1,
        |state| state.position == end_pos,
        |state| state.expand(&mut valley),
    );

    state_2.timestep
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
