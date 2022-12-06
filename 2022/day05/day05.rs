#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone)]
struct Stacks(Vec<Vec<u8>>);

#[derive(Debug)]
struct Instructions(Vec<(usize, usize, usize)>);

fn parse_input(input: String) -> (Stacks, Instructions) {
    let lines: Vec<_> = input.lines().collect();
    let split_index = lines.iter().position(|line| line.is_empty()).unwrap();

    let crate_lines = &lines[..split_index - 1];
    let number_line = lines[split_index - 1];
    let instruction_lines = &lines[split_index + 1..];

    let stack_count = (number_line.len() + 3) / 4;
    let mut stacks = vec![Vec::new(); stack_count];
    for line in crate_lines.iter().rev() {
        let line = line.as_bytes();
        for i in 0..stack_count {
            if line[4 * i + 1].is_ascii_alphabetic() {
                stacks[i].push(line[4 * i + 1]);
            }
        }
    }

    let mut instructions = Vec::new();
    for line in instruction_lines {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        let count = split[1].parse().unwrap();
        let from = split[3].parse().unwrap();
        let to = split[5].parse().unwrap();
        instructions.push((count, from, to));
    }

    (Stacks(stacks), Instructions(instructions))
}

fn get_mut_refs<T>(arr: &mut [T], i: usize, j: usize) -> Option<(&mut T, &mut T)> {
    // Safe implementation of getting two mutable references to different
    // elements in a vec. It's written like this in a slightly awkward way to
    // avoid emitting a bounds check
    // It's not great because unlike the most optimal version of this code it
    // still generates a bunch of branches.
    if i >= arr.len() || j >= arr.len() {
        return None;
    }

    if i < j {
        let (first, second) = arr.split_at_mut(j);
        Some((&mut first[i], &mut second[0]))
    } else if j < i {
        let (first, second) = arr.split_at_mut(i);
        Some((&mut second[0], &mut first[j]))
    } else {
        None
    }
}

/*
// Here's the ideal version of get_mut_refs which is implemented using unsafe.
fn get_mut_refs_unsafe<T>(arr: &mut [T], i: usize, j: usize) -> Option<(&mut T, &mut T)> {
    if i == j || i >= arr.len() || j >= arr.len() {
        None
    } else {
        unsafe {
            let ptr = arr.as_mut_ptr();
            let first = &mut *ptr.add(i);
            let second = &mut *ptr.add(j);
            Some((first, second))
        }
    }
}
*/

fn star1((stacks, instructions): &(Stacks, Instructions)) -> String {
    let mut stacks = stacks.0.clone();
    for (count, from, to) in instructions.0.iter().copied() {
        let (from, to) = get_mut_refs(&mut stacks, from - 1, to - 1).unwrap();
        to.extend(from.drain(from.len() - count..).rev());
    }

    String::from_utf8(stacks.iter().filter_map(|x| x.last().copied()).collect()).unwrap()
}

fn star2((stacks, instructions): &(Stacks, Instructions)) -> String {
    let mut stacks = stacks.0.clone();
    for (count, from, to) in instructions.0.iter().copied() {
        let (from, to) = get_mut_refs(&mut stacks, from - 1, to - 1).unwrap();
        to.extend(from.drain(from.len() - count..));
    }

    String::from_utf8(stacks.iter().filter_map(|x| x.last().copied()).collect()).unwrap()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
