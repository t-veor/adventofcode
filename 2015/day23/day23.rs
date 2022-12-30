#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

// Looking at the program input, it looks like the program first loads two
// different numbers depending on whether a = 1 or not into a, and then computes
// the length of the Collatz chain of a and outputs it in b.
// So I could just do this problem by hand. But that's no fun, you gotta
// simulate it

use std::ops::{Index, IndexMut};

#[derive(Debug, Clone, Copy)]
enum Register {
    A,
    B,
}

struct RegFile([u64; 2]);

impl Index<Register> for RegFile {
    type Output = u64;

    fn index(&self, index: Register) -> &Self::Output {
        &self.0[match index {
            Register::A => 0,
            Register::B => 1,
        }]
    }
}

impl IndexMut<Register> for RegFile {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        &mut self.0[match index {
            Register::A => 0,
            Register::B => 1,
        }]
    }
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Hlf(Register),
    Tpl(Register),
    Inc(Register),
    Jmp(isize),
    Jie(Register, isize),
    Jio(Register, isize),
}

fn run_program(instructions: &[Instruction], mut regfile: RegFile) -> RegFile {
    let mut pc = 0;
    while let Some(instruction) = pc
        .try_into()
        .ok()
        .and_then(|pc: usize| instructions.get(pc))
    {
        match *instruction {
            Instruction::Hlf(r) => {
                regfile[r] /= 2;
                pc += 1;
            }
            Instruction::Tpl(r) => {
                regfile[r] *= 3;
                pc += 1;
            }
            Instruction::Inc(r) => {
                regfile[r] += 1;
                pc += 1;
            }
            Instruction::Jmp(offset) => pc += offset,
            Instruction::Jie(r, offset) => {
                if regfile[r] % 2 == 0 {
                    pc += offset;
                } else {
                    pc += 1;
                }
            }
            Instruction::Jio(r, offset) => {
                if regfile[r] == 1 {
                    pc += offset;
                } else {
                    pc += 1;
                }
            }
        }
    }

    regfile
}

fn parse_input(input: String) -> Vec<Instruction> {
    fn reg(r: &str) -> Register {
        if r == "a" {
            Register::A
        } else {
            Register::B
        }
    }

    fn extract(line: &str) -> Instruction {
        let (opcode, args) = line.split_once(' ').unwrap();
        match opcode {
            "hlf" => Instruction::Hlf(reg(args)),
            "tpl" => Instruction::Tpl(reg(args)),
            "inc" => Instruction::Inc(reg(args)),
            "jmp" => Instruction::Jmp(args.parse().unwrap()),
            "jie" => {
                let (r, offset) = args.split_once(", ").unwrap();
                Instruction::Jie(reg(r), offset.parse().unwrap())
            }
            "jio" => {
                let (r, offset) = args.split_once(", ").unwrap();
                Instruction::Jio(reg(r), offset.parse().unwrap())
            }
            _ => unreachable!(),
        }
    }

    input.lines().map(extract).collect()
}

fn star1(input: &[Instruction]) -> u64 {
    run_program(input, RegFile([0, 0])).0[1]
}

fn star2(input: &[Instruction]) -> u64 {
    run_program(input, RegFile([1, 0])).0[1]
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
