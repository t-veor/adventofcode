#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    ops::{Index, IndexMut},
    str::FromStr,
};

#[derive(Debug, Clone, Copy)]
enum Register {
    A,
    B,
    C,
    D,
}

#[derive(Debug, Clone, Copy)]
enum Src {
    Reg(Register),
    Imm(i64),
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Cpy(Src, Register),
    Inc(Register),
    Dec(Register),
    Jnz(Src, i64),
}

impl FromStr for Register {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "a" => Self::A,
            "b" => Self::B,
            "c" => Self::C,
            "d" => Self::D,
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
struct RegFile([i64; 4]);

impl Index<Register> for RegFile {
    type Output = i64;

    fn index(&self, index: Register) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<Register> for RegFile {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl RegFile {
    fn execute_program(&mut self, program: &[Instruction]) {
        let mut pc = 0isize;
        while (0..program.len() as isize).contains(&pc) {
            match program[pc as usize] {
                Instruction::Cpy(src, dst) => {
                    let src = match src {
                        Src::Reg(r) => self[r],
                        Src::Imm(v) => v,
                    };
                    self[dst] = src;
                }
                Instruction::Inc(dst) => self[dst] += 1,
                Instruction::Dec(dst) => self[dst] -= 1,
                Instruction::Jnz(src, offset) => {
                    let src = match src {
                        Src::Reg(r) => self[r],
                        Src::Imm(v) => v,
                    };
                    if src != 0 {
                        pc += offset as isize;
                        continue;
                    }
                }
            }
            pc += 1;
        }
    }
}

fn parse_input(input: &str) -> Vec<Instruction> {
    fn extract(line: &str) -> Instruction {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        match split[0] {
            "cpy" => {
                let src = if let Ok(x) = split[1].parse() {
                    Src::Imm(x)
                } else {
                    Src::Reg(split[1].parse().unwrap())
                };
                let dst = split[2].parse().unwrap();
                Instruction::Cpy(src, dst)
            }
            "inc" => Instruction::Inc(split[1].parse().unwrap()),
            "dec" => Instruction::Dec(split[1].parse().unwrap()),
            "jnz" => {
                let src = if let Ok(x) = split[1].parse() {
                    Src::Imm(x)
                } else {
                    Src::Reg(split[1].parse().unwrap())
                };
                Instruction::Jnz(src, split[2].parse().unwrap())
            }
            _ => unreachable!(),
        }
    }

    input.lines().map(extract).collect()
}

fn star1(input: &[Instruction]) -> i64 {
    let mut regfile = RegFile([0; 4]);
    regfile.execute_program(input);
    regfile[Register::A]
}

fn star2(input: &[Instruction]) -> i64 {
    let mut regfile = RegFile([0, 0, 1, 0]);
    regfile.execute_program(input);
    regfile[Register::A]
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
