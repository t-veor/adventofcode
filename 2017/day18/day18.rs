#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{collections::VecDeque, str::FromStr};

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
enum DuetInstruction {
    Snd(DuetOperand),
    Set(DuetReg, DuetOperand),
    Add(DuetReg, DuetOperand),
    Mul(DuetReg, DuetOperand),
    Mod(DuetReg, DuetOperand),
    Rcv(DuetReg),
    Jgz(DuetOperand, DuetOperand),
}

impl FromStr for DuetInstruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split = s.split(' ').collect::<Vec<_>>();
        match *split.get(0).ok_or(())? {
            "snd" => Ok(Self::Snd(split.get(1).ok_or(())?.parse()?)),
            "set" => Ok(Self::Set(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            "add" => Ok(Self::Add(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            "mul" => Ok(Self::Mul(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            "mod" => Ok(Self::Mod(
                split.get(1).ok_or(())?.parse()?,
                split.get(2).ok_or(())?.parse()?,
            )),
            "rcv" => Ok(Self::Rcv(split.get(1).ok_or(())?.parse()?)),
            "jgz" => Ok(Self::Jgz(
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

    send_count: u32,
}

impl DuetProgram {
    fn new(instructions: Vec<DuetInstruction>) -> Self {
        Self {
            reg: [0; 26],
            instructions: instructions,
            pc: 0,

            send_count: 0,
        }
    }

    fn resolve(&self, operand: DuetOperand) -> i64 {
        match operand {
            DuetOperand::Reg(r) => self.reg[r.index()],
            DuetOperand::Imm(x) => x,
        }
    }

    fn execute_one(
        &mut self,
        send: impl FnOnce(i64),
        receive: impl FnOnce(&mut Self, DuetReg) -> bool,
    ) -> bool {
        let instruction = self.instructions[self.pc as usize];

        match instruction {
            DuetInstruction::Snd(a) => {
                self.send_count += 1;
                send(self.resolve(a))
            }
            DuetInstruction::Set(a, b) => self.reg[a.index()] = self.resolve(b),
            DuetInstruction::Add(a, b) => self.reg[a.index()] += self.resolve(b),
            DuetInstruction::Mul(a, b) => self.reg[a.index()] *= self.resolve(b),
            DuetInstruction::Mod(a, b) => self.reg[a.index()] %= self.resolve(b),
            DuetInstruction::Rcv(a) => {
                if !receive(self, a) {
                    return false;
                }
            }
            DuetInstruction::Jgz(a, b) => {
                if self.resolve(a) > 0 {
                    self.pc += self.resolve(b) as isize - 1;
                }
            }
        }

        self.pc += 1;

        true
    }
}

fn parse_input(input: &str) -> Vec<DuetInstruction> {
    input
        .lines()
        .map(|line| line.trim().parse().unwrap())
        .collect()
}

fn star1(input: &[DuetInstruction]) -> i64 {
    let mut program = DuetProgram::new(input.to_vec());
    let mut last_received = 0;
    while program.execute_one(|x| last_received = x, |this, r| this.reg[r.index()] == 0) {}
    last_received
}

fn star2(input: &[DuetInstruction]) -> u32 {
    let mut programs = (
        DuetProgram::new(input.to_vec()),
        DuetProgram::new(input.to_vec()),
    );
    programs.1.reg[DuetReg(b'p').index()] = 1;

    let mut queues = (VecDeque::new(), VecDeque::new());

    let mut switches_without_progress = 0;

    let (mut curr_program, mut other_program) = (&mut programs.0, &mut programs.1);
    let (mut curr_queue, mut other_queue) = (&mut queues.0, &mut queues.1);

    loop {
        if curr_program.execute_one(
            |x| other_queue.push_back(x),
            |this, reg| {
                if let Some(val) = curr_queue.pop_front() {
                    this.reg[reg.index()] = val;
                    true
                } else {
                    false
                }
            },
        ) {
            switches_without_progress = 0;
        } else {
            switches_without_progress += 1;
            std::mem::swap(&mut curr_program, &mut other_program);
            std::mem::swap(&mut curr_queue, &mut other_queue);
        }

        if switches_without_progress >= 2 {
            // deadlock
            break;
        }
    }

    programs.1.send_count
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
