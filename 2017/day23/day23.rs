#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DuetReg(u8);

impl DuetReg {
    fn index(self) -> usize {
        (self.0 - b'a') as usize
    }
}

impl FromStr for DuetReg {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let byte = s.as_bytes().first().ok_or(())?;
        if byte.is_ascii_lowercase() {
            Ok(Self(*byte))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DuetOperand {
    Reg(DuetReg),
    Imm(i64),
}

impl FromStr for DuetOperand {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse()
            .map(Self::Reg)
            .or_else(|_| s.parse().map(Self::Imm).map_err(|_| ()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DuetInstruction {
    Set(DuetReg, DuetOperand),
    Sub(DuetReg, DuetOperand),
    Mul(DuetReg, DuetOperand),
    Jnz(DuetOperand, DuetOperand),

    // Special instructions generated by optimisation
    IsPrime(DuetReg, DuetOperand),
    Noop,
}

impl FromStr for DuetInstruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split = s.split(' ').collect::<Vec<_>>();
        match *split.get(0).ok_or(())? {
            "set" => Ok(Self::Set(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            "sub" => Ok(Self::Sub(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            "mul" => Ok(Self::Mul(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            "jnz" => Ok(Self::Jnz(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
struct DuetProgram {
    reg: [i64; 26],
    instructions: Vec<DuetInstruction>,
    pc: isize,

    mul_count: u32,
}

impl DuetProgram {
    fn new(instructions: Vec<DuetInstruction>) -> Self {
        Self {
            reg: [0; 26],
            instructions: instructions,
            pc: 0,

            mul_count: 0,
        }
    }

    fn resolve(&self, operand: DuetOperand) -> i64 {
        match operand {
            DuetOperand::Reg(r) => self.reg[r.index()],
            DuetOperand::Imm(x) => x,
        }
    }

    fn execute_one(&mut self) {
        let instruction = self.instructions[self.pc as usize];

        match instruction {
            DuetInstruction::Set(a, b) => self.reg[a.index()] = self.resolve(b),
            DuetInstruction::Sub(a, b) => self.reg[a.index()] -= self.resolve(b),
            DuetInstruction::Mul(a, b) => {
                self.mul_count += 1;
                self.reg[a.index()] *= self.resolve(b)
            }
            DuetInstruction::Jnz(a, b) => {
                if self.resolve(a) != 0 {
                    self.pc += self.resolve(b) as isize - 1;
                }
            }

            DuetInstruction::IsPrime(a, b) => {
                self.reg[a.index()] = is_prime(self.resolve(b)) as i64
            }
            DuetInstruction::Noop => (),
        }

        self.pc += 1;
    }

    fn execute(&mut self) {
        while (0..self.instructions.len() as isize).contains(&self.pc) {
            self.execute_one()
        }
    }
}

fn is_prime(n: i64) -> bool {
    if n == 2 || n == 3 {
        return true;
    } else if n <= 1 || n % 2 == 0 || n % 3 == 0 {
        return false;
    }

    let mut i = 5;
    while i * i < n {
        if n % i == 0 || n % (i + 2) == 0 {
            return false;
        }

        i += 6;
    }

    true
}

fn parse_input(input: &str) -> Vec<DuetInstruction> {
    input
        .lines()
        .map(|line| line.trim().parse().unwrap())
        .collect()
}

fn star1(input: &[DuetInstruction]) -> u32 {
    let mut program = DuetProgram::new(input.to_vec());
    program.execute();

    program.mul_count
}

fn star2(input: &[DuetInstruction]) -> i64 {
    // The program counts the number of composite numbers (for my input) between
    // of the form x = 109900 + 17n, where 109900 <= x <= 126900, in a really
    // inefficient way.

    // To "optimise" it I'm going to optimistically assume that all inputs will
    // have this same primality checking code here, which I will simply optimise
    // down into a single IsPrime instruction.

    let needle = parse_input(
        "set f 1
    set d 2
    set e 2
    set g d
    mul g e
    sub g b
    jnz g 2
    set f 0
    sub e -1
    set g e
    sub g b
    jnz g -8
    sub d -1
    set g d
    sub g b
    jnz g -13",
    );

    let mut instructions = input.to_vec();

    for (i, possible) in instructions.windows(needle.len()).enumerate() {
        if possible == &needle {
            (&mut instructions[i..i + needle.len()]).fill(DuetInstruction::Noop);
            instructions[i] =
                DuetInstruction::IsPrime(DuetReg(b'f'), DuetOperand::Reg(DuetReg(b'b')));
            break;
        }
    }

    let mut program = DuetProgram::new(instructions);
    program.reg[DuetReg(b'a').index()] = 1;

    program.execute();

    program.reg[DuetReg(b'h').index()]
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
