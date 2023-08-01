#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: &str) -> Vec<i32> {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn star1(input: &[i32]) -> usize {
    let mut instrs = input.to_vec();
    let mut pc = 0isize;
    let mut steps = 0;

    while pc >= 0 && (pc as usize) < instrs.len() {
        let jmp = instrs[pc as usize] as isize;
        instrs[pc as usize] += 1;
        pc += jmp;
        steps += 1;
    }

    steps
}

fn star2(input: &[i32]) -> i32 {
    let mut instrs = input.to_vec();
    let mut pc = 0isize;
    let mut steps = 0;

    while pc >= 0 && (pc as usize) < instrs.len() {
        let jmp = instrs[pc as usize] as isize;
        if jmp >= 3 {
            instrs[pc as usize] -= 1;
        } else {
            instrs[pc as usize] += 1;
        }
        pc += jmp;
        steps += 1;
    }

    steps
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
