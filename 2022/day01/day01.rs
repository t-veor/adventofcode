#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```
fn parse_input(input: String) -> Vec<Vec<i32>> {
    let mut elves = Vec::new();
    let mut curr_elf = Vec::new();

    for line in input.lines() {
        match line.parse() {
            Ok(x) => curr_elf.push(x),
            Err(_) => {
                elves.push(curr_elf);
                curr_elf = Vec::new();
            }
        }
    }

    if !curr_elf.is_empty() {
        elves.push(curr_elf);
    }

    elves
}

fn star1(elves: &[Vec<i32>]) -> i32 {
    elves
        .iter()
        .map(|elf| elf.iter().copied().sum())
        .max()
        .unwrap()
}

fn star2(elves: &[Vec<i32>]) -> i32 {
    // I already did a solution where I sorted and grabbed the top 3 in the
    // Python solution, so let's do one with O(n) runtime and O(1) scratch space
    let mut top_three = [Option::<i32>::None; 3];
    let summed_elves = elves.iter().map(|elf| elf.iter().copied().sum());

    for elf in summed_elves {
        if Some(elf) > top_three[0] {
            top_three[0] = Some(elf);
            for i in 0..2 {
                if top_three[i] > top_three[i + 1] {
                    top_three.swap(i, i + 1);
                }
            }
        }
    }

    top_three.into_iter().filter_map(|x| x).sum()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let elves = parse_input(input);
    println!("{}", star1(&elves));
    println!("{}", star2(&elves));
}
