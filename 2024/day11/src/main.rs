use std::collections::HashMap;

use utils::read_input_file;

fn try_split_stone(stone: u64) -> Option<(u64, u64)> {
    let digits = stone.max(1).ilog10() + 1;

    if digits % 2 == 0 {
        let m = 10u64.pow(digits / 2);
        Some((stone / m, stone % m))
    } else {
        None
    }
}

#[derive(Debug, Default, Clone)]
struct Stones {
    counts: HashMap<u64, u64>,
}

impl Stones {
    fn add_stones(&mut self, stone: u64, count: u64) {
        *self.counts.entry(stone).or_insert(0) += count;
    }

    fn blink(&self) -> Self {
        let mut new_stones = Self::default();

        for (&stone, &prev_count) in self.counts.iter() {
            if stone == 0 {
                new_stones.add_stones(1, prev_count);
            } else if let Some((left, right)) = try_split_stone(stone) {
                new_stones.add_stones(left, prev_count);
                new_stones.add_stones(right, prev_count);
            } else {
                new_stones.add_stones(stone * 2024, prev_count);
            }
        }

        new_stones
    }

    fn total(&self) -> u64 {
        self.counts.values().sum()
    }
}

fn parse_input(input: &str) -> Stones {
    let mut stones = Stones::default();
    for stone in input.split_whitespace().map(|i| i.parse().unwrap()) {
        stones.add_stones(stone, 1);
    }

    stones
}

fn star1(stones: &Stones) -> u64 {
    let mut stones = stones.clone();

    for _ in 0..25 {
        stones = stones.blink();
    }

    stones.total()
}

fn star2(stones: &Stones) -> u64 {
    let mut stones = stones.clone();

    for _ in 0..75 {
        stones = stones.blink();
    }

    stones.total()
}

fn main() {
    let input = read_input_file!();
    let stones = parse_input(&input);

    println!("{}", star1(&stones));
    println!("{}", star2(&stones));
}
