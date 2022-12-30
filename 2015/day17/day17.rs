#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::collections::HashMap;

fn parse_input(input: String) -> Vec<i32> {
    let mut result: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();
    result.sort_unstable_by(|a, b| b.cmp(a));
    result
}

fn generate_sums(input: &[i32], remaining: i32) -> Vec<Vec<i32>> {
    fn recurse(
        input: &[i32],
        start_idx: usize,
        remaining: i32,
        cache: &mut HashMap<(usize, i32), Vec<Vec<i32>>>,
    ) -> Vec<Vec<i32>> {
        if let Some(result) = cache.get(&(start_idx, remaining)) {
            return result.clone();
        }

        let result = if remaining == 0 {
            vec![vec![]]
        } else if start_idx >= input.len() {
            vec![]
        } else {
            let mut ways = vec![];
            if input[start_idx] <= remaining {
                ways.extend(
                    recurse(input, start_idx + 1, remaining - input[start_idx], cache)
                        .into_iter()
                        .map(|mut vec| {
                            vec.push(input[start_idx]);
                            vec
                        }),
                );
            }
            ways.extend_from_slice(&recurse(input, start_idx + 1, remaining, cache));

            ways
        };

        cache.insert((start_idx, remaining), result.clone());
        result
    }

    recurse(input, 0, remaining, &mut HashMap::new())
}

fn star1(input: &[i32]) -> usize {
    generate_sums(input, 150).len()
}

fn star2(input: &[i32]) -> usize {
    let sums = generate_sums(input, 150);
    let min_length = sums.iter().map(|s| s.len()).min().unwrap();
    sums.iter().filter(|s| s.len() == min_length).count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
