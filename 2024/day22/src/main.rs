use std::collections::{HashMap, HashSet};

use utils::read_input_file;

fn prng_step(mut n: u32) -> u32 {
    const MASK: u32 = (1 << 24) - 1;

    n = (n ^ n << 6) & MASK;
    n = (n ^ n >> 5) & MASK;
    n = (n ^ n << 11) & MASK;

    n
}

fn get_prices(seed: u32, count: usize) -> Vec<i8> {
    let mut prices = Vec::with_capacity(count + 1);
    prices.push((seed % 10) as i8);

    let mut n = seed;
    for _ in 0..count {
        n = prng_step(n);
        prices.push((n % 10) as i8);
    }

    prices
}

fn find_optimal_sequence(seeds: &[u32], price_count: usize, price_diffs: usize) -> u64 {
    let mut table = HashMap::new();

    for &seed in seeds {
        let prices = get_prices(seed, price_count);

        let mut already_sold = HashSet::new();

        for window in prices.windows(price_diffs + 1) {
            let diffs: Vec<_> = window
                .windows(2)
                .map(|window| window[1] - window[0])
                .collect();

            if already_sold.insert(diffs.clone()) {
                *table.entry(diffs).or_insert(0) += *window.last().unwrap() as u64;
            }
        }
    }

    *table.values().max().unwrap()
}

fn parse_input(input: &str) -> Vec<u32> {
    input.lines().map(|i| i.parse().unwrap()).collect()
}

fn star1(seeds: &[u32]) -> u64 {
    let mut total = 0;

    for &seed in seeds {
        let mut n = seed;
        for _ in 0..2000 {
            n = prng_step(n);
        }

        total += n as u64;
    }

    total
}

fn star2(seeds: &[u32]) -> u64 {
    find_optimal_sequence(seeds, 2000, 4)
}

fn main() {
    let input = read_input_file!();
    let seeds = parse_input(&input);

    println!("{}", star1(&seeds));
    println!("{}", star2(&seeds));
}
