#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

#[derive(Debug, Clone, Copy)]
enum Direction {
    Forward,
    Left,
    Right,
    Back,
}

trait GridState: Default + Clone + Copy + Display {
    fn next(self) -> (Self, Direction);
    fn is_infected(self) -> bool;
}

#[derive(Debug, Clone, Copy, Default)]
enum Part1State {
    #[default]
    Clean,
    Infected,
}

impl Display for Part1State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            Self::Clean => '.',
            Self::Infected => '#',
        })
    }
}

impl GridState for Part1State {
    fn next(self) -> (Self, Direction) {
        match self {
            Self::Clean => (Self::Infected, Direction::Left),
            Self::Infected => (Self::Clean, Direction::Right),
        }
    }

    fn is_infected(self) -> bool {
        matches!(self, Self::Infected)
    }
}

#[derive(Debug, Clone, Copy, Default)]
enum Part2State {
    #[default]
    Clean,
    Weakened,
    Infected,
    Flagged,
}

impl Display for Part2State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            Self::Clean => '.',
            Self::Weakened => 'W',
            Self::Infected => '#',
            Self::Flagged => 'F',
        })
    }
}

impl GridState for Part2State {
    fn next(self) -> (Self, Direction) {
        match self {
            Self::Clean => (Self::Weakened, Direction::Left),
            Self::Weakened => (Self::Infected, Direction::Forward),
            Self::Infected => (Self::Flagged, Direction::Right),
            Self::Flagged => (Self::Clean, Direction::Back),
        }
    }

    fn is_infected(self) -> bool {
        matches!(self, Self::Infected)
    }
}

impl From<Part1State> for Part2State {
    fn from(value: Part1State) -> Self {
        match value {
            Part1State::Clean => Self::Clean,
            Part1State::Infected => Self::Infected,
        }
    }
}

#[derive(Debug, Clone)]
struct LangtonsAnt<S: GridState> {
    grid: HashMap<(i32, i32), S>,
    position: (i32, i32),
    dir: (i32, i32),

    infections: u32,
}

impl<S: GridState> LangtonsAnt<S> {
    fn step(&mut self) {
        let state = self.grid.entry(self.position).or_default();

        let (next_state, direction) = state.next();

        let (dx, dy) = self.dir;
        self.dir = match direction {
            Direction::Forward => (dx, dy),
            Direction::Left => (dy, -dx),
            Direction::Right => (-dy, dx),
            Direction::Back => (-dx, -dy),
        };

        *state = next_state;

        if next_state.is_infected() {
            self.infections += 1;
        }

        self.position.0 += self.dir.0;
        self.position.1 += self.dir.1;
    }

    #[allow(unused)]
    fn debug_print(&self) {
        let min_x = self.grid.keys().map(|(x, _)| *x).min().unwrap_or(0);
        let max_x = self.grid.keys().map(|(x, _)| *x).max().unwrap_or(0);
        let min_y = self.grid.keys().map(|(_, y)| *y).min().unwrap_or(0);
        let max_y = self.grid.keys().map(|(_, y)| *y).max().unwrap_or(0);

        for y in min_y..=max_y {
            for x in min_x..=max_x {
                if self.position == (x, y) {
                    print!("[");
                } else {
                    print!(" ");
                }

                print!("{}", self.grid.get(&(x, y)).copied().unwrap_or_default());

                if self.position == (x, y) {
                    print!("]");
                } else {
                    print!(" ");
                }
            }
            println!();
        }
    }
}

fn parse_input(input: &str) -> LangtonsAnt<Part1State> {
    let mut grid = HashMap::new();

    let width = input.lines().next().unwrap().trim().len();
    let height = input.lines().count();

    for (y, line) in input.lines().enumerate() {
        for (x, &c) in line.as_bytes().iter().enumerate() {
            if c == b'#' {
                grid.insert((x as i32, y as i32), Part1State::Infected);
            }
        }
    }

    LangtonsAnt {
        grid,
        position: (width as i32 / 2, height as i32 / 2),
        dir: (0, -1),
        infections: 0,
    }
}

fn star1(input: &LangtonsAnt<Part1State>) -> u32 {
    let mut ant = input.clone();
    for _ in 0..10000 {
        ant.step();
    }

    ant.infections
}

fn star2(input: &LangtonsAnt<Part1State>) -> u32 {
    let mut ant = LangtonsAnt::<Part2State> {
        grid: input.grid.iter().map(|(&k, &v)| (k, v.into())).collect(),
        position: input.position,
        dir: input.dir,

        infections: 0,
    };

    for i in 0..10_000_000 {
        ant.step();
    }

    ant.infections
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
