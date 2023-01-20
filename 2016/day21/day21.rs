#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

#[derive(Debug, Clone, Copy)]
enum Instruction {
    SwapPosition(usize, usize),
    SwapLetter(u8, u8),
    RotateLeft(usize),
    RotateRight(usize),
    RotateOnLetter(u8),
    Reverse(usize, usize),
    Move(usize, usize),
}

fn parse_input(input: &str) -> Vec<Instruction> {
    fn extract(line: &str) -> Instruction {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        if split[0] == "swap" {
            if split[1] == "position" {
                Instruction::SwapPosition(split[2].parse().unwrap(), split[5].parse().unwrap())
            } else {
                Instruction::SwapLetter(split[2].as_bytes()[0], split[5].as_bytes()[0])
            }
        } else if split[0] == "rotate" {
            if split[1] == "left" {
                Instruction::RotateLeft(split[2].parse().unwrap())
            } else if split[1] == "right" {
                Instruction::RotateRight(split[2].parse().unwrap())
            } else {
                Instruction::RotateOnLetter(split[6].as_bytes()[0])
            }
        } else if split[0] == "reverse" {
            Instruction::Reverse(split[2].parse().unwrap(), split[4].parse().unwrap())
        } else {
            Instruction::Move(split[2].parse().unwrap(), split[5].parse().unwrap())
        }
    }

    input.lines().map(extract).collect()
}

fn execute_instructions(instructions: &[Instruction], string: &str) -> String {
    let mut string = string.as_bytes().to_vec();
    for &instruction in instructions {
        match instruction {
            Instruction::SwapPosition(i, j) => string.swap(i, j),
            Instruction::SwapLetter(x, y) => {
                let i = string.iter().position(|&c| c == x).unwrap();
                let j = string.iter().position(|&c| c == y).unwrap();
                string.swap(i, j)
            }
            Instruction::RotateLeft(i) => string.rotate_left(i),
            Instruction::RotateRight(j) => string.rotate_right(j),
            Instruction::RotateOnLetter(x) => {
                let pos = string.iter().position(|&c| c == x).unwrap();
                let rotations = (1 + pos + if pos >= 4 { 1 } else { 0 }) % string.len();
                string.rotate_right(rotations)
            }
            Instruction::Reverse(i, j) => string[i..=j].reverse(),
            Instruction::Move(i, j) => {
                if i < j {
                    string[i..=j].rotate_left(1)
                } else {
                    string[j..=i].rotate_right(1)
                }
            }
        }
    }

    String::from_utf8(string).unwrap()
}

fn reverse_instructions(instructions: &[Instruction], string: &str) -> String {
    let mut string = string.as_bytes().to_vec();
    'outer: for &instruction in instructions.iter().rev() {
        match instruction {
            Instruction::SwapPosition(i, j) => string.swap(i, j),
            Instruction::SwapLetter(x, y) => {
                let i = string.iter().position(|&c| c == x).unwrap();
                let j = string.iter().position(|&c| c == y).unwrap();
                string.swap(i, j)
            }
            Instruction::RotateLeft(i) => string.rotate_right(i),
            Instruction::RotateRight(j) => string.rotate_left(j),
            Instruction::RotateOnLetter(x) => {
                // Let a be the initial position of the letter x and b be its
                // final position. We know y, and we know that it is the result
                // of rotating starting from a (1 + a + a >= 4 ? 1 : 0) times.
                // So we want to solve:
                // 2a + 1 (+ 1) = b (mod string.len())
                // I'm not sure this can actually be solved explicitly, since
                // 2 doesn't necessarily have a multiplicative inverse (the
                // input string's length is 8), so let's just iterate through
                // 0..string.len() and take the first position that works. (If
                // there are multiple, it means the password could unscramble to
                // multiple different passwords that give the same result
                // anyway.)
                let b = string.iter().position(|&c| c == x).unwrap();
                for a in 0..string.len() {
                    let rotations = (1 + a + if a >= 4 { 1 } else { 0 }) % string.len();
                    let rotated_position = (a + rotations) % string.len();
                    if rotated_position == b {
                        string.rotate_left(rotations);
                        continue 'outer;
                    }
                }
                panic!("No position found to work for reversing rotate on letter instruction")
            }
            Instruction::Reverse(i, j) => string[i..=j].reverse(),
            Instruction::Move(i, j) => {
                if i < j {
                    string[i..=j].rotate_right(1)
                } else {
                    string[j..=i].rotate_left(1)
                }
            }
        }
    }

    String::from_utf8(string).unwrap()
}

fn star1(input: &[Instruction]) -> String {
    execute_instructions(input, "abcdefgh")
}

fn star2(input: &[Instruction]) -> String {
    reverse_instructions(input, "fbgdceah")
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
