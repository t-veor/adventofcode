#!/usr/bin/env run-cargo-script

use std::str::FromStr;

#[derive(Debug)]
enum Command {
    Forward(i32),
    Down(i32),
    Up(i32),
}

impl FromStr for Command {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .split_once(' ')
            .ok_or(())
            .and_then(|(command, magnitude)| {
                let magnitude: i32 = magnitude.parse().map_err(|_| ())?;
                match command {
                    "forward" => Ok(Self::Forward(magnitude)),
                    "down" => Ok(Self::Down(magnitude)),
                    "up" => Ok(Self::Up(magnitude)),
                    _ => Err(()),
                }
            })
    }
}

fn star1(input: &[Command]) -> i32 {
    let mut x = 0;
    let mut y = 0;
    for command in input {
        match command {
            Command::Forward(n) => x += n,
            Command::Down(n) => y += n,
            Command::Up(n) => y -= n,
        }
    }
    x * y
}

fn star2(input: &[Command]) -> i32 {
    let mut x = 0;
    let mut y = 0;
    let mut aim = 0;
    for command in input {
        match command {
            Command::Forward(n) => {
                x += n;
                y += n * aim;
            }
            Command::Down(n) => aim += n,
            Command::Up(n) => aim -= n,
        }
    }
    x * y
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input: Vec<_> = std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|i| i.parse().unwrap())
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
