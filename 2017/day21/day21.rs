#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    fmt::{Display, Write},
    str::FromStr,
};

#[derive(Debug, Clone)]
struct Grid {
    size: usize,
    grid: Vec<Vec<bool>>,
}

impl Grid {
    fn rotate_90(&self) -> Self {
        let size = self.size;
        let mut new = vec![vec![false; size]; size];

        for i in 0..size {
            for j in 0..size {
                new[j][size - i - 1] = self.grid[i][j]
            }
        }

        Self { size, grid: new }
    }

    fn flip(&self) -> Self {
        let mut new = self.clone();
        new.grid.iter_mut().for_each(|row| row.reverse());
        new
    }

    fn compact(&self) -> u16 {
        let mut result = 0;
        for row in self.grid.iter().rev() {
            for &val in row.iter().rev() {
                result <<= 1;
                result |= val as u16;
            }
        }
        result
    }

    fn from_compact(mut x: u16, size: usize) -> Self {
        let mut grid = vec![vec![false; size]; size];
        for i in 0..size {
            for j in 0..size {
                grid[i][j] = x % 2 == 1;
                x >>= 1;
            }
        }
        Self { grid, size }
    }

    fn subgrids(&self, subgrid_size: usize) -> impl Iterator<Item = Grid> + '_ {
        assert!(self.size % subgrid_size == 0);

        self.grid
            .chunks_exact(subgrid_size)
            .flat_map(move |row_chunk| {
                (0..self.size / subgrid_size).map(move |i| {
                    let mut grid = Vec::with_capacity(subgrid_size);
                    for row in row_chunk {
                        grid.push(row[i * subgrid_size..(i + 1) * subgrid_size].to_vec());
                    }

                    Self {
                        grid,
                        size: subgrid_size,
                    }
                })
            })
    }

    fn stitch(subgrids: impl Iterator<Item = Grid>, size: usize) -> Self {
        let mut grid = vec![vec![false; size]; size];

        let mut row = 0;
        let mut col = 0;
        for subgrid in subgrids {
            for i in 0..subgrid.size {
                for j in 0..subgrid.size {
                    grid[row + i][col + j] = subgrid.grid[i][j];
                }
            }
            col += subgrid.size;
            if col >= size {
                col = 0;
                row += subgrid.size;
            }
        }

        Self { grid, size }
    }

    fn enhance(&self, rules: &RuleMap) -> Self {
        let (subgrid_size, new_size) = if self.size % 2 == 0 {
            (2, self.size / 2 * 3)
        } else {
            (3, self.size / 3 * 4)
        };

        Self::stitch(
            self.subgrids(subgrid_size).map(|grid| rules.map(&grid)),
            new_size,
        )
    }

    fn count(&self) -> usize {
        self.grid
            .iter()
            .flat_map(|row| row.iter().map(|&i| i as usize))
            .sum()
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in &self.grid {
            for &val in row {
                f.write_char(if val { '#' } else { '.' })?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl FromStr for Grid {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let grid: Vec<Vec<bool>> = s
            .split('/')
            .map(|row| row.as_bytes().iter().map(|&x| x == b'#').collect())
            .collect();

        let size = grid.len();
        for row in &grid {
            if row.len() != size {
                return Err(());
            }
        }

        Ok(Self { size, grid })
    }
}

struct RuleMap {
    two_rules: [u16; 16],
    three_rules: [u16; 512],
}

impl RuleMap {
    fn map(&self, grid: &Grid) -> Grid {
        if grid.size == 2 {
            Grid::from_compact(self.two_rules[grid.compact() as usize], 3)
        } else {
            Grid::from_compact(self.three_rules[grid.compact() as usize], 4)
        }
    }
}

impl FromIterator<(Grid, Grid)> for RuleMap {
    fn from_iter<T: IntoIterator<Item = (Grid, Grid)>>(iter: T) -> Self {
        let mut two_rules = [0; 16];
        let mut three_rules = [0; 512];

        for (from, to) in iter {
            let to = to.compact();
            let map = if from.size == 2 {
                &mut two_rules[..]
            } else {
                &mut three_rules[..]
            };

            let mut curr = from.clone();
            for _ in 0..4 {
                map[curr.compact() as usize] = to;
                curr = curr.rotate_90();
            }
            curr = curr.flip();
            for _ in 0..4 {
                map[curr.compact() as usize] = to;
                curr = curr.rotate_90();
            }
        }

        Self {
            two_rules,
            three_rules,
        }
    }
}

fn parse_input(input: &str) -> RuleMap {
    fn parse_line(line: &str) -> (Grid, Grid) {
        let (from, to) = line.trim().split_once(" => ").unwrap();
        (from.parse().unwrap(), to.parse().unwrap())
    }

    input.lines().map(parse_line).collect()
}

fn star1(input: &RuleMap) -> usize {
    let mut grid = ".#./..#/###".parse::<Grid>().unwrap();

    for _ in 0..5 {
        grid = grid.enhance(input);
    }

    grid.count()
}

fn star2(input: &RuleMap) -> usize {
    let mut grid = ".#./..#/###".parse::<Grid>().unwrap();

    for _ in 0..18 {
        grid = grid.enhance(input);
    }

    grid.count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
