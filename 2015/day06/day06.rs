#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone, Copy)]
enum InstructionType {
    TurnOn,
    TurnOff,
    Toggle,
}

#[derive(Debug, Clone)]
struct Instruction {
    instruction_type: InstructionType,
    from: (usize, usize),
    to: (usize, usize),
}

fn parse_input(input: String) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| {
            let split: Vec<_> = line.split_ascii_whitespace().collect();
            let (instruction_type, from, to) = if split[0] == "toggle" {
                (InstructionType::Toggle, split[1], split[3])
            } else {
                (
                    if split[1] == "on" {
                        InstructionType::TurnOn
                    } else {
                        InstructionType::TurnOff
                    },
                    split[2],
                    split[4],
                )
            };

            let into_tuple = |s: &str| {
                let mut iter = s.split(',').map(|i| i.parse().unwrap());
                (iter.next().unwrap(), iter.next().unwrap())
            };

            Instruction {
                instruction_type,
                from: into_tuple(from),
                to: into_tuple(to),
            }
        })
        .collect()
}

fn star1(input: &[Instruction]) -> usize {
    let mut lights = vec![false; 1000 * 1000];

    for instruction in input {
        let Instruction {
            instruction_type,
            from,
            to,
        } = *instruction;

        for y in from.1..=to.1 {
            for x in from.0..=to.0 {
                lights[x + 1000 * y] = match instruction_type {
                    InstructionType::TurnOn => true,
                    InstructionType::TurnOff => false,
                    InstructionType::Toggle => !lights[x + 1000 * y],
                }
            }
        }
    }

    lights.into_iter().filter(|x| *x).count()
}

fn star2(input: &[Instruction]) -> u32 {
    let mut lights = vec![0u32; 1000 * 1000];

    for instruction in input {
        let Instruction {
            instruction_type,
            from,
            to,
        } = *instruction;

        for y in from.1..=to.1 {
            for x in from.0..=to.0 {
                let val = &mut lights[x + 1000 * y];
                match instruction_type {
                    InstructionType::TurnOn => *val += 1,
                    InstructionType::TurnOff => *val = val.saturating_sub(1),
                    InstructionType::Toggle => *val += 2,
                }
            }
        }
    }

    lights.iter().sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
