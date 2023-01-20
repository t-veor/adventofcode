#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{
    ops::{Index, IndexMut},
    str::FromStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    A,
    B,
    C,
    D,
}

#[derive(Debug, Clone, Copy)]
enum RegOrImm {
    Reg(Register),
    Imm(i64),
}

impl FromStr for RegOrImm {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(s) = s.parse() {
            Ok(Self::Imm(s))
        } else if let Ok(r) = s.parse() {
            Ok(Self::Reg(r))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Cpy(RegOrImm, Register),
    Inc(Register),
    Dec(Register),
    Jnz(RegOrImm, RegOrImm),
    Tgl(RegOrImm),

    // Special instruction indicating that an invalid instruction was generated
    // from toggling and to skip execution this cycle
    Nop,
    // Optimised instructions
    Add(RegOrImm, Register),
    Mul(RegOrImm, Register),
}

impl Instruction {
    fn toggle(&self) -> Self {
        match *self {
            Instruction::Cpy(src, dst) => Instruction::Jnz(src, RegOrImm::Reg(dst)),
            Instruction::Inc(reg) => Instruction::Dec(reg),
            Instruction::Dec(reg) => Instruction::Inc(reg),
            Instruction::Jnz(src, dst) => {
                if let RegOrImm::Reg(reg) = dst {
                    Instruction::Cpy(src, reg)
                } else {
                    Instruction::Nop
                }
            }
            Instruction::Tgl(dst) => {
                if let RegOrImm::Reg(reg) = dst {
                    Instruction::Inc(reg)
                } else {
                    Instruction::Nop
                }
            }
            Instruction::Nop => Instruction::Nop,

            Instruction::Add(_, _) => Instruction::Nop,
            Instruction::Mul(_, _) => Instruction::Nop,
        }
    }
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

// Rust is fast enough at CPU work that part 2 runs without any optimisations.
// Still, it's such a shame of a wasted puzzle, let's implement some peephole
// optimisation anyway

fn peephole_optimize(instructions: &[Instruction], toggle_state: &[bool]) -> Vec<Instruction> {
    fn all_different(reg: &[Register]) -> bool {
        for i in 0..reg.len() {
            for j in i + 1..reg.len() {
                if reg[i] == reg[j] {
                    return false;
                }
            }
        }
        true
    }

    let mut result: Vec<_> = instructions
        .iter()
        .enumerate()
        .map(|(i, instr)| {
            if toggle_state[i] {
                instr.toggle()
            } else {
                *instr
            }
        })
        .collect();

    let mut current = &mut result[..];
    loop {
        match current {
            // cpy b c; inc a; dec c; jnz c -2; dec d; jnz d -5
            // ->
            // mul b d; add d a; cpy 0 c; cpy 0 d; nop; nop
            #[rustfmt::skip]
            &mut [
                Instruction::Cpy(b, c),
                Instruction::Inc(a),
                Instruction::Dec(c_),
                Instruction::Jnz(RegOrImm::Reg(c__), RegOrImm::Imm(-2)),
                Instruction::Dec(d),
                Instruction::Jnz(RegOrImm::Reg(d_),
                RegOrImm::Imm(-5)),
                ..
            ]
                if c == c_
                    && c == c__
                    && d == d_
                    && if let RegOrImm::Reg(b) = b {
                        all_different(&[a, b, c, d])
                    } else {
                        all_different(&[a, c, d])
                    } =>
            {
                current[..6].copy_from_slice(&[
                    Instruction::Mul(b, d),
                    Instruction::Add(RegOrImm::Reg(d), a),
                    Instruction::Cpy(RegOrImm::Imm(0), c),
                    Instruction::Cpy(RegOrImm::Imm(0), d),
                    Instruction::Nop,
                    Instruction::Nop,
                ]);
                current = &mut current[6..];
            }
            // inc a; dec b; jnz b -2
            // ->
            // add b a; cpy 0 b; nop
            #[rustfmt::skip]
            &mut [
                Instruction::Inc(a),
                Instruction::Dec(b),
                Instruction::Jnz(RegOrImm::Reg(b_), RegOrImm::Imm(-2)),
                ..
            ] if b == b_ && all_different(&[a, b]) => {
                current[..3].copy_from_slice(&[
                    Instruction::Add(RegOrImm::Reg(b), a),
                    Instruction::Cpy(RegOrImm::Imm(0), b),
                    Instruction::Nop,
                ]);
                current = &mut current[3..];
            }
            [_, rest @ ..] => current = rest,
            [] => break,
        }
    }

    result
}

impl RegFile {
    fn resolve(&self, reg_or_imm: RegOrImm) -> i64 {
        match reg_or_imm {
            RegOrImm::Reg(r) => self[r],
            RegOrImm::Imm(v) => v,
        }
    }

    fn execute_program(&mut self, program: &[Instruction]) {
        let mut pc = 0isize;
        let mut toggle_states = vec![false; program.len()];

        let mut optimised_instructions = peephole_optimize(&program, &toggle_states);

        while (0..optimised_instructions.len() as isize).contains(&pc) {
            match optimised_instructions[pc as usize] {
                Instruction::Cpy(src, dst) => {
                    self[dst] = self.resolve(src);
                }
                Instruction::Inc(dst) => self[dst] += 1,
                Instruction::Dec(dst) => self[dst] -= 1,
                Instruction::Jnz(src, offset) => {
                    let src = self.resolve(src);
                    let offset = self.resolve(offset);
                    if src != 0 {
                        pc += offset as isize;
                        continue;
                    }
                }
                Instruction::Tgl(dst) => {
                    let dst = self.resolve(dst);
                    let target = pc + dst as isize;
                    if target >= 0 && (target as usize) < toggle_states.len() {
                        if toggle_states[target as usize] {
                            panic!("toggling an instruction twice. It's not clear what this does from the problem statement");
                        }
                        toggle_states[target as usize] = !toggle_states[target as usize];
                        // Recompute optimised instructions because instructions may be toggled
                        optimised_instructions = peephole_optimize(&program, &toggle_states);
                    }
                }
                Instruction::Nop => (),
                Instruction::Add(src, dst) => {
                    let src = self.resolve(src);
                    self[dst] += src;
                }
                Instruction::Mul(src, dst) => {
                    let src = self.resolve(src);
                    self[dst] *= src;
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
                let src = split[1].parse().unwrap();
                let dst = split[2].parse().unwrap();
                Instruction::Cpy(src, dst)
            }
            "inc" => Instruction::Inc(split[1].parse().unwrap()),
            "dec" => Instruction::Dec(split[1].parse().unwrap()),
            "jnz" => {
                let src = split[1].parse().unwrap();
                let dst = split[2].parse().unwrap();
                Instruction::Jnz(src, dst)
            }
            "tgl" => Instruction::Tgl(split[1].parse().unwrap()),
            _ => unreachable!(),
        }
    }

    input.lines().map(extract).collect()
}

fn star1(input: &[Instruction]) -> i64 {
    let mut regfile = RegFile([7, 0, 0, 0]);
    regfile.execute_program(input);
    regfile[Register::A]
}

fn star2(input: &[Instruction]) -> i64 {
    let mut regfile = RegFile([12, 0, 0, 0]);
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
