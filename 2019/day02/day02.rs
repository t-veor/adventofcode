#!/usr/bin/env run-cargo-script
fn run_program(input: &Vec<usize>, noun: usize, verb: usize) -> usize {
    let mut memory = input.clone();
    memory[1] = noun;
    memory[2] = verb;

    let mut pc = 0;
    loop {
        let opcode = memory[pc];
        if opcode == 99 {
            break
        }

        let src = memory[pc+1];
        let trg = memory[pc+2];
        let dst = memory[pc+3];
        let op1 = memory[src];
        let op2 = memory[trg];

        memory[dst] = match opcode {
            1 => op1 + op2,
            2 => op1 * op2,
            _ => panic!(format!("Unknown opcode {}", opcode)),
        };

        pc += 4;
    }

    memory[0]
}

fn star1(input: &Vec<usize>) -> usize {
    run_program(input, 12, 2)
}

fn star2(input: &Vec<usize>) -> usize {
    let target = 19690720;

    for noun in 0..100 {
        for verb in 0..100 {
            if run_program(input, noun, verb) == target {
                return 100 * noun + verb
            }
        }
    }

    panic!("No solution found!")
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
