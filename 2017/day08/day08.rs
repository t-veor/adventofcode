#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

#[derive(Debug, Default)]
struct RegFile {
    values: HashMap<String, i32>,
}

#[derive(Debug)]
enum Action {
    Inc,
    Dec,
}

#[derive(Debug)]
enum Condition {
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
}

#[derive(Debug)]
struct Instruction {
    target: String,
    action: Action,
    immediate: i32,
    comparand: String,
    condition: Condition,
    comparator: i32,
}

fn parse_input(input: &str) -> Vec<Instruction> {
    fn parse_line(line: &str) -> Instruction {
        let split: Vec<_> = line.split_ascii_whitespace().collect();

        let target = split[0].to_owned();
        let action = match split[1] {
            "inc" => Action::Inc,
            "dec" => Action::Dec,
            x => panic!("Unknown action {x}"),
        };
        let immediate = split[2].parse().unwrap();

        let comparand = split[4].to_owned();
        let condition = match split[5] {
            "<" => Condition::Lt,
            ">" => Condition::Gt,
            "<=" => Condition::Leq,
            ">=" => Condition::Geq,
            "==" => Condition::Eq,
            "!=" => Condition::Neq,
            x => panic!("Unknwn condition {x}"),
        };
        let comparator = split[6].parse().unwrap();

        Instruction {
            target,
            action,
            immediate,
            comparand,
            condition,
            comparator,
        }
    }

    input.lines().map(parse_line).collect()
}

fn exec_instr(instruction: &Instruction, regfile: &mut RegFile) -> Option<i32> {
    let comparand = regfile
        .values
        .get(&instruction.comparand)
        .copied()
        .unwrap_or(0);
    let comparator = instruction.comparator;
    let enable = match instruction.condition {
        Condition::Lt => comparand < comparator,
        Condition::Gt => comparand > comparator,
        Condition::Leq => comparand <= comparator,
        Condition::Geq => comparand >= comparator,
        Condition::Eq => comparand == comparator,
        Condition::Neq => comparand != comparator,
    };

    if enable {
        let target = regfile
            .values
            .entry(instruction.target.clone())
            .or_insert(0);

        match instruction.action {
            Action::Inc => *target += instruction.immediate,
            Action::Dec => *target -= instruction.immediate,
        }

        Some(*target)
    } else {
        None
    }
}

fn star1(input: &[Instruction]) -> i32 {
    let mut regfile = RegFile::default();

    for instruction in input {
        exec_instr(instruction, &mut regfile);
    }

    *regfile.values.values().max().unwrap()
}

fn star2(input: &[Instruction]) -> i32 {
    let mut regfile = RegFile::default();

    input
        .iter()
        .filter_map(|i| exec_instr(i, &mut regfile))
        .max()
        .unwrap_or(0)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
