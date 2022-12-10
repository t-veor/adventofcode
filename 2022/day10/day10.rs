#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone, Copy)]
enum Instruction {
    AddX(i32),
    Noop,
}

struct RegisterHistory(Vec<i32>);

impl RegisterHistory {
    fn execute_program(program: &[Instruction]) -> Self {
        let mut x = 1;
        let mut cycles = 0;

        let mut history = vec![1];

        for instruction in program {
            match instruction {
                Instruction::AddX(v) => {
                    x += v;
                    cycles += 2;
                    while history.len() < cycles {
                        history.push(*history.last().unwrap())
                    }
                    history.push(x)
                }
                Instruction::Noop => cycles += 1,
            }
        }

        Self(history)
    }

    fn get_during(&self, cycle: usize) -> i32 {
        self.0
            .get(cycle.saturating_sub(1))
            .or(self.0.last())
            .copied()
            .unwrap_or(1)
    }
}

fn parse_input(input: String) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| {
            let split: Vec<_> = line.split_ascii_whitespace().collect();
            match split[0] {
                "addx" => Instruction::AddX(split[1].parse().unwrap()),
                "noop" => Instruction::Noop,
                _ => unreachable!(),
            }
        })
        .collect()
}

fn star1(input: &[Instruction]) -> i32 {
    let history = RegisterHistory::execute_program(input);
    (20..=220)
        .step_by(40)
        .map(|i| {
            let reg = history.get_during(i);
            reg * i as i32
        })
        .sum()
}

fn star2(input: &[Instruction]) {
    let history = RegisterHistory::execute_program(input);
    for i in (0..240).step_by(40) {
        for j in 1..=40 {
            let sprite_pos = history.get_during(i + j as usize);
            if (sprite_pos..sprite_pos + 3).contains(&j) {
                print!("#");
            } else {
                print!(" ");
            }
        }
        println!()
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    star2(&input);
}
