#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

// Literally Conway's Game of Life, lol

const WIDTH: usize = 100;
const HEIGHT: usize = 100;

fn parse_input(input: String) -> Vec<bool> {
    let mut grid = Vec::with_capacity(WIDTH * HEIGHT);
    assert_eq!(input.lines().count(), HEIGHT);
    for line in input.lines() {
        assert_eq!(line.len(), WIDTH);
        for c in line.as_bytes() {
            grid.push(*c == b'#');
        }
    }

    grid
}

fn step(grid: &[bool], corners_stuck_on: bool) -> Vec<bool> {
    assert_eq!(grid.len(), WIDTH * HEIGHT);
    let mut new_grid = Vec::with_capacity(WIDTH * HEIGHT);

    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            if corners_stuck_on && (x == 0 || x == WIDTH - 1) && (y == 0 || y == HEIGHT - 1) {
                new_grid.push(true);
                continue;
            }

            let mut neighbors = 0;
            for dy in [-1, 0, 1] {
                for dx in [-1, 0, 1] {
                    if dx == 0 && dy == 0 {
                        continue;
                    }
                    let (x_, y_) = (x as isize + dx, y as isize + dy);
                    if x_ < 0 || y_ < 0 || x_ >= WIDTH as isize || y_ >= HEIGHT as isize {
                        continue;
                    }
                    let (x_, y_) = (x_ as usize, y_ as usize);
                    if grid[y_ * WIDTH + x_] {
                        neighbors += 1;
                    }
                }
            }

            new_grid.push(neighbors == 3 || neighbors == 2 && grid[y * WIDTH + x]);
        }
    }

    new_grid
}

fn star1(input: &[bool]) -> usize {
    let mut state = input.to_vec();
    for _ in 0..100 {
        state = step(&state, false);
    }

    state.iter().filter(|&&x| x).count()
}

fn star2(input: &[bool]) -> usize {
    let mut state = input.to_vec();
    for _ in 0..100 {
        state = step(&state, true);
    }

    state.iter().filter(|&&x| x).count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
