#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn delta(&self) -> (i32, i32) {
        match self {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }
}

const KEYPAD: [[char; 3]; 3] = [['1', '2', '3'], ['4', '5', '6'], ['7', '8', '9']];

fn parse_input(input: String) -> Vec<Vec<Direction>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|char| match char {
                    'U' => Direction::Up,
                    'D' => Direction::Down,
                    'L' => Direction::Left,
                    'R' => Direction::Right,
                    _ => unreachable!(),
                })
                .collect()
        })
        .collect()
}

fn star1(input: &[Vec<Direction>]) -> String {
    let mut result = String::new();
    let (mut x, mut y) = (1, 1);

    for line in input {
        for dir in line {
            let (dx, dy) = dir.delta();
            x = (x + dx).clamp(0, 2);
            y = (y + dy).clamp(0, 2);
        }
        result.push(KEYPAD[y as usize][x as usize]);
    }

    result
}

const KEYPAD2: [[Option<char>; 5]; 5] = [
    [None, None, Some('1'), None, None],
    [None, Some('2'), Some('3'), Some('4'), None],
    [Some('5'), Some('6'), Some('7'), Some('8'), Some('9')],
    [None, Some('A'), Some('B'), Some('C'), None],
    [None, None, Some('D'), None, None],
];

fn star2(input: &[Vec<Direction>]) -> String {
    let mut result = String::new();
    let (mut x, mut y) = (0, 2);

    for line in input {
        for dir in line {
            let (dx, dy) = dir.delta();
            let x_ = (x + dx).clamp(0, 4);
            let y_ = (y + dy).clamp(0, 4);
            if KEYPAD2[y_ as usize][x_ as usize].is_some() {
                (x, y) = (x_, y_);
            }
        }
        result.push(KEYPAD2[y as usize][x as usize].unwrap());
    }

    result
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
