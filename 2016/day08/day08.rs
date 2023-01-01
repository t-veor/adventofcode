#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::fmt::{Display, Write};

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Rect { width: usize, height: usize },
    RotateRow { y: usize, amount: usize },
    RotateColumn { x: usize, amount: usize },
}

#[derive(Debug)]
struct Screen {
    screen: Vec<bool>,
    width: usize,
    height: usize,
}

impl Screen {
    fn new(width: usize, height: usize) -> Self {
        Self {
            screen: vec![false; width * height],
            width,
            height,
        }
    }

    fn rect(&mut self, width: usize, height: usize) {
        for y in 0..height {
            self.screen[y * self.width..y * self.width + width].fill(true);
        }
    }

    fn rotate_row(&mut self, y: usize, amount: usize) {
        let row = &mut self.screen[y * self.width..(y + 1) * self.width];
        row.rotate_right(amount);
    }

    fn rotate_column(&mut self, x: usize, amount: usize) {
        let mut column = Vec::with_capacity(self.height);
        for y in 0..self.height {
            column.push(self.screen[y * self.width + x]);
        }
        column.rotate_right(amount);
        for (y, val) in column.into_iter().enumerate() {
            self.screen[y * self.width + x] = val;
        }
    }

    fn process_instructions(&mut self, instructions: &[Instruction]) {
        for &instruction in instructions {
            match instruction {
                Instruction::Rect { width, height } => self.rect(width, height),
                Instruction::RotateRow { y, amount } => self.rotate_row(y, amount),
                Instruction::RotateColumn { x, amount } => self.rotate_column(x, amount),
            }
        }
    }

    fn count(&self) -> usize {
        self.screen.iter().filter(|x| **x).count()
    }
}

impl Display for Screen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                f.write_char(if self.screen[y * self.width + x] {
                    '#'
                } else {
                    ' '
                })?;
            }
            if y < self.height - 1 {
                f.write_char('\n')?;
            }
        }
        Ok(())
    }
}

fn parse_input(input: &str) -> Vec<Instruction> {
    fn extract(line: &str) -> Instruction {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        if split[0] == "rect" {
            let (width, height) = split[1].split_once('x').unwrap();
            Instruction::Rect {
                width: width.parse().unwrap(),
                height: height.parse().unwrap(),
            }
        } else if split[1] == "row" {
            let y = split[2].strip_prefix("y=").unwrap().parse().unwrap();
            let amount = split[4].parse().unwrap();
            Instruction::RotateRow { y, amount }
        } else {
            let x = split[2].strip_prefix("x=").unwrap().parse().unwrap();
            let amount = split[4].parse().unwrap();
            Instruction::RotateColumn { x, amount }
        }
    }

    input.lines().map(extract).collect()
}

fn star1(input: &[Instruction]) -> usize {
    let mut screen = Screen::new(50, 6);
    screen.process_instructions(input);
    screen.count()
}

fn star2(input: &[Instruction]) -> String {
    let mut screen = Screen::new(50, 6);
    screen.process_instructions(input);
    screen.to_string()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
