#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

struct Spinlock {
    next: Vec<usize>,
    curr_pos: usize,
    step_count: usize,
}

impl Spinlock {
    fn new(step_count: usize) -> Self {
        Self {
            next: vec![0],
            curr_pos: 0,
            step_count,
        }
    }

    fn step_and_insert(&mut self) {
        let next = self.next.len();
        let step_count = self.step_count % next;

        for _ in 0..step_count {
            self.curr_pos = self.next[self.curr_pos];
        }

        self.next.push(self.next[self.curr_pos]);
        self.next[self.curr_pos] = next;
        self.curr_pos = next;
    }
}

fn parse_input(input: &str) -> usize {
    input.trim().parse().unwrap()
}

fn star1(input: usize) -> usize {
    let mut spinlock = Spinlock::new(input);

    for _ in 0..2017 {
        spinlock.step_and_insert();
    }

    spinlock.next[spinlock.curr_pos]
}

fn star2(input: usize) -> usize {
    let step_count = input;
    let mut curr_position = 0;
    let mut last_inserted_after_0 = 0;

    for i in 1..=50_000_000 {
        curr_position += step_count;
        curr_position %= i;

        if curr_position == 0 {
            last_inserted_after_0 = i;
        }
        curr_position += 1;
        curr_position %= i + 1;
    }

    last_inserted_after_0
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(input));
    println!("{}", star2(input));
}
