#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Bot { id: i32, low: Target, high: Target },
    Init { value: i32, target: Target },
}

#[derive(Debug, Clone, Copy)]
enum Target {
    Bot(i32),
    Output(i32),
}

fn parse_input(input: &str) -> Vec<Instruction> {
    fn extract(line: &str) -> Instruction {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        let make_target = |idx: usize| {
            let constructor = if split[idx] == "bot" {
                Target::Bot
            } else {
                Target::Output
            };
            constructor(split[idx + 1].parse().unwrap())
        };

        if split[0] == "bot" {
            let id = split[1].parse().unwrap();
            let low = make_target(5);
            let high = make_target(10);

            Instruction::Bot { id, low, high }
        } else {
            let value = split[1].parse().unwrap();
            let target = make_target(4);
            Instruction::Init { value, target }
        }
    }

    input.lines().map(extract).collect()
}

#[derive(Debug, Default)]
struct Bot {
    targets: Option<(Target, Target)>,
    holding: Vec<i32>,
}

#[derive(Debug)]
struct BotProcessResult {
    processed_bot: i32,
    sent_low: (Target, i32),
    sent_high: (Target, i32),
}

#[derive(Debug)]
struct Bots {
    bots: HashMap<i32, Bot>,
    outputs: HashMap<i32, Vec<i32>>,
    to_update: Vec<i32>,
}

impl Bots {
    fn new(instructions: &[Instruction]) -> Self {
        let mut bots = Self {
            bots: Default::default(),
            outputs: Default::default(),
            to_update: Default::default(),
        };

        for &instr in instructions {
            match instr {
                Instruction::Bot { id, low, high } => bots.set_targets(id, low, high),
                Instruction::Init { value, target } => bots.push_to_target(target, value),
            }
        }

        bots
    }

    fn set_targets(&mut self, bot: i32, low: Target, high: Target) {
        self.bots.entry(bot).or_default().targets = Some((low, high))
    }

    fn push_to_target(&mut self, target: Target, value: i32) {
        match target {
            Target::Bot(id) => self.push_to_bot(id, value),
            Target::Output(id) => self.push_to_output(id, value),
        }
    }

    fn push_to_bot(&mut self, id: i32, value: i32) {
        let bot = self.bots.entry(id).or_default();
        bot.holding.push(value);
        if bot.holding.len() == 2 {
            self.to_update.push(id);
        }
    }

    fn push_to_output(&mut self, id: i32, value: i32) {
        self.outputs.entry(id).or_default().push(value)
    }

    fn process_one_step(&mut self) -> Option<BotProcessResult> {
        let (id, bot) = loop {
            let id = self.to_update.pop()?;
            let bot = self.bots.get_mut(&id).unwrap();
            if bot.targets.is_some() {
                break (id, bot);
            }
        };

        let (low_target, high_target) = bot.targets.unwrap();
        let (a, b) = (bot.holding[0], bot.holding[1]);
        bot.holding.drain(..2);
        if bot.holding.len() >= 2 {
            // Push this bot back onto the queue as we need to process it more
            self.to_update.push(id);
        }

        let (low, high) = (a.min(b), a.max(b));
        self.push_to_target(low_target, low);
        self.push_to_target(high_target, high);

        Some(BotProcessResult {
            processed_bot: id,
            sent_low: (low_target, low),
            sent_high: (high_target, high),
        })
    }
}

fn star1(input: &[Instruction]) -> i32 {
    let mut bots = Bots::new(input);

    while let Some(result) = bots.process_one_step() {
        if result.sent_low.1 == 17 && result.sent_high.1 == 61 {
            return result.processed_bot;
        }
    }

    panic!("did not find bot who compared 17 & 61")
}

fn star2(input: &[Instruction]) -> i32 {
    let mut bots = Bots::new(input);
    while bots.process_one_step().is_some() {}

    let a = bots.outputs[&0].first().unwrap();
    let b = bots.outputs[&1].first().unwrap();
    let c = bots.outputs[&2].first().unwrap();
    a * b * c
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
