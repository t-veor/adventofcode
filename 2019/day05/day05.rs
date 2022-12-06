#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```
struct IntCodeVM {
    memory: Vec<i32>,
    pc: usize,
    halted: bool,
}

enum VMStatus {
    Halted,
    Output(i32),
    Continue,
}

type DecodeOut = (i32, (usize, usize, usize), (i32, i32, i32));

impl IntCodeVM {
    fn new(memory: Vec<i32>) -> Self {
        Self {
            memory,
            pc: 0,
            halted: false,
        }
    }

    fn fetch_decode(&mut self) -> DecodeOut {
        let read = |i: usize| {
            if i < self.memory.len() {
                self.memory[i]
            } else {
                0
            }
        };

        let op = read(self.pc);
        let opcode = op % 100;
        let src_mode = (op % 1000) / 100;
        let trg_mode = (op % 10000) / 1000;
        let dst_mode = (op % 100000) / 10000;

        let src = read(self.pc + 1);
        let trg = read(self.pc + 2);
        let dst = read(self.pc + 3);

        let op1 = if src_mode == 1 {
            src
        } else {
            read(src as usize)
        };
        let op2 = if trg_mode == 1 {
            trg
        } else {
            read(trg as usize)
        };
        let op3 = if dst_mode == 1 {
            dst
        } else {
            read(dst as usize)
        };

        (
            opcode,
            (src as usize, trg as usize, dst as usize),
            (op1, op2, op3),
        )
    }

    fn step<F>(&mut self, next_input: F) -> VMStatus
    where
        F: FnOnce() -> i32,
    {
        if self.halted {
            return VMStatus::Halted;
        }

        let (opcode, (src, _trg, dst), (op1, op2, _op3)) = self.fetch_decode();

        match opcode {
            99 => VMStatus::Halted,
            1 => {
                self.memory[dst] = op1 + op2;
                self.pc += 4;
                VMStatus::Continue
            }
            2 => {
                self.memory[dst] = op1 * op2;
                self.pc += 4;
                VMStatus::Continue
            }
            3 => {
                self.memory[src] = next_input();
                self.pc += 2;
                VMStatus::Continue
            }
            4 => {
                self.pc += 2;
                VMStatus::Output(op1)
            }
            5 => {
                self.pc = if op1 != 0 { op2 as usize } else { self.pc + 3 };
                VMStatus::Continue
            }
            6 => {
                self.pc = if op1 == 0 { op2 as usize } else { self.pc + 3 };
                VMStatus::Continue
            }
            7 => {
                self.memory[dst] = (op1 < op2) as i32;
                self.pc += 4;
                VMStatus::Continue
            }
            8 => {
                self.memory[dst] = (op1 == op2) as i32;
                self.pc += 4;
                VMStatus::Continue
            }
            _ => panic!("Unknown opcode {}", opcode),
        }
    }

    fn run_to_complete(&mut self, inputs: &Vec<i32>) -> Vec<i32> {
        let mut i = 0;
        let mut outputs = Vec::new();

        loop {
            let status = self.step(|| {
                let result = inputs[i];
                i += 1;
                result
            });

            match status {
                VMStatus::Halted => break,
                VMStatus::Output(x) => outputs.push(x),
                VMStatus::Continue => (),
            }
        }

        outputs
    }
}

fn star1(program: &Vec<i32>) -> i32 {
    let mut vm = IntCodeVM::new(program.clone());
    let output = vm.run_to_complete(&vec![1]);
    output[output.len() - 1]
}

fn star2(program: &Vec<i32>) -> i32 {
    let mut vm = IntCodeVM::new(program.clone());
    let output = vm.run_to_complete(&vec![5]);
    output[0]
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename)
        .unwrap()
        .trim()
        .split(",")
        .map(|i| i.parse().unwrap())
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
