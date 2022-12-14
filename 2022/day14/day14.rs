#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{collections::HashMap, iter::Inspect, time::Instant};

const SOURCE_POS: (i32, i32) = (500, 0);

#[derive(Debug, Clone, Copy)]
enum GridSquare {
    Rock,
    Sand,
}

#[derive(Debug, Clone)]
struct SandGrid {
    grid: HashMap<(i32, i32), GridSquare>,
}

impl SandGrid {
    #[allow(unused)]
    fn debug_print(&self) {
        let mut min_x = i32::MAX;
        let mut min_y = i32::MAX;
        let mut max_x = i32::MIN;
        let mut max_y = i32::MIN;

        for &(x, y) in self.grid.keys().chain(&[SOURCE_POS]) {
            min_x = x.min(min_x);
            min_y = y.min(min_x);
            max_x = x.max(max_x);
            max_y = y.max(max_y);
        }

        for y in min_y..=max_y {
            for x in min_x..=max_x {
                if (x, y) == SOURCE_POS {
                    print!("+");
                } else {
                    let square = self.grid.get(&(x, y));

                    match square {
                        Some(GridSquare::Rock) => print!("#"),
                        Some(GridSquare::Sand) => print!("o"),
                        None => print!("."),
                    }
                }
            }
            println!()
        }
    }

    fn is_free(&self, pos: (i32, i32)) -> bool {
        !self.grid.contains_key(&pos)
    }

    fn is_sand(&self, pos: (i32, i32)) -> bool {
        match self.grid.get(&pos) {
            Some(GridSquare::Sand) => true,
            _ => false,
        }
    }

    fn drop_sand(&mut self) -> bool {
        let max_y = self.grid.keys().map(|(_, y)| *y).max().unwrap_or(0);

        let (mut x, mut y) = SOURCE_POS;
        while y <= max_y {
            if self.is_free((x, y + 1)) {
                y += 1;
            } else if self.is_free((x - 1, y + 1)) {
                x -= 1;
                y += 1;
            } else if self.is_free((x + 1, y + 1)) {
                x += 1;
                y += 1;
            } else {
                self.grid.insert((x, y), GridSquare::Sand);
                return true;
            }
        }

        false
    }

    fn fill_from_floor(&mut self) -> i32 {
        let max_y = self.grid.keys().map(|(_, y)| *y).max().unwrap_or(0);
        let floor_height = max_y + 2;

        self.grid.insert(SOURCE_POS, GridSquare::Sand);
        let mut sand_count = 1;

        for y in 1..floor_height {
            for x in SOURCE_POS.0 - y..=SOURCE_POS.0 + y {
                if self.is_free((x, y))
                    && (self.is_sand((x - 1, y - 1))
                        || self.is_sand((x, y - 1))
                        || self.is_sand((x + 1, y - 1)))
                {
                    self.grid.insert((x, y), GridSquare::Sand);
                    sand_count += 1;
                }
            }
        }

        sand_count
    }
}

fn parse_input(input: String) -> SandGrid {
    let mut grid = HashMap::new();

    for line in input.lines() {
        let coords: Vec<(i32, i32)> = line
            .split(" -> ")
            .map(|p| {
                let (x, y) = p.split_once(',').unwrap();
                (x.parse().unwrap(), y.parse().unwrap())
            })
            .collect();

        for window in coords.windows(2) {
            let start = window[0];
            let end = window[1];
            if start.0 == end.0 {
                let x = start.0;
                let start_y = start.1.min(end.1);
                let end_y = start.1.max(end.1);
                for y in start_y..=end_y {
                    grid.insert((x, y), GridSquare::Rock);
                }
            } else {
                let y = start.1;
                let start_x = start.0.min(end.0);
                let end_x = start.0.max(end.0);
                for x in start_x..=end_x {
                    grid.insert((x, y), GridSquare::Rock);
                }
            }
        }
    }

    SandGrid { grid }
}

fn star1(input: &SandGrid) -> i32 {
    let mut grid = input.clone();

    let mut count = 0;
    while grid.drop_sand() {
        count += 1;
    }

    count
}

fn star2(input: &SandGrid) -> i32 {
    let mut grid = input.clone();
    grid.fill_from_floor()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
