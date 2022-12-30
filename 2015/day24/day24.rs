#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

// We can reuse the day 17 code for generating all possible ways to sum to an
// value.

use std::collections::HashMap;

fn parse_input(input: String) -> Vec<i64> {
    let mut result: Vec<i64> = input.lines().map(|x| x.parse().unwrap()).collect();
    result.sort_unstable_by(|a, b| b.cmp(a));
    result
}

fn generate_sums(input: &[i64], remaining: i64) -> Vec<Vec<i64>> {
    fn recurse(
        input: &[i64],
        start_idx: usize,
        remaining: i64,
        cache: &mut HashMap<(usize, i64), Vec<Vec<i64>>>,
    ) -> Vec<Vec<i64>> {
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

fn check_sum_is_possible(input: &[i64], remaining: i64) -> bool {
    fn recurse(
        input: &[i64],
        start_idx: usize,
        remaining: i64,
        cache: &mut HashMap<(usize, i64), bool>,
    ) -> bool {
        if let Some(result) = cache.get(&(start_idx, remaining)) {
            return result.clone();
        }

        let result = if remaining == 0 {
            true
        } else if start_idx >= input.len() {
            false
        } else {
            let mut possible = false;
            if input[start_idx] <= remaining {
                possible |= recurse(input, start_idx + 1, remaining - input[start_idx], cache);
            }
            possible || recurse(input, start_idx + 1, remaining, cache)
        };

        cache.insert((start_idx, remaining), result.clone());
        result
    }

    recurse(input, 0, remaining, &mut HashMap::new())
}

fn group_by_len(mut xss: Vec<Vec<i64>>) -> Vec<Vec<Vec<i64>>> {
    xss.sort_unstable_by_key(|xs| xs.len());

    let mut result = Vec::new();
    let mut curr_group: Vec<Vec<i64>> = Vec::new();
    for xs in xss {
        if curr_group.is_empty() || curr_group[0].len() == xs.len() {
            curr_group.push(xs);
        } else {
            result.push(curr_group);
            curr_group = vec![xs];
        }
    }
    if !curr_group.is_empty() {
        result.push(curr_group);
    }

    result
}

fn star1(input: &[i64]) -> i64 {
    let target = input.iter().sum::<i64>() / 3;
    let center_combos = generate_sums(input, target);
    let center_combos_grouped = group_by_len(center_combos);

    for groups_with_same_len in center_combos_grouped {
        let mut min_quantum_entanglement = None;
        for candidate_grouping in groups_with_same_len {
            let mut remaining_numbers = input.to_vec();
            remaining_numbers.retain(|x| !candidate_grouping.contains(x));

            if check_sum_is_possible(&remaining_numbers, target) {
                let quantum_entanglement = candidate_grouping.iter().product::<i64>();
                if min_quantum_entanglement.is_none()
                    || quantum_entanglement < min_quantum_entanglement.unwrap()
                {
                    min_quantum_entanglement = Some(quantum_entanglement);
                }
            }
        }

        if let Some(min_quantum_entanglement) = min_quantum_entanglement {
            return min_quantum_entanglement;
        }
    }

    panic!("no valid configs found")
}

fn star2(input: &[i64]) -> i64 {
    // This is horrible but it produces the right answer
    let target = input.iter().sum::<i64>() / 4;
    let center_combos = generate_sums(input, target);
    let center_combos_grouped = group_by_len(center_combos);

    for groups_with_same_len in center_combos_grouped {
        let mut min_quantum_entanglement = None;
        for candidate_grouping in groups_with_same_len {
            let mut remaining_numbers = input.to_vec();
            remaining_numbers.retain(|x| !candidate_grouping.contains(x));

            let second_groups = generate_sums(&remaining_numbers, target);
            let mut possible = false;
            for second_group in second_groups {
                let mut remaining_numbers_2 = remaining_numbers.clone();
                remaining_numbers_2.retain(|x| !second_group.contains(x));

                if check_sum_is_possible(&remaining_numbers_2, target) {
                    possible = true;
                    break;
                }
            }

            if possible {
                let quantum_entanglement = candidate_grouping.iter().product::<i64>();
                if min_quantum_entanglement.is_none()
                    || quantum_entanglement < min_quantum_entanglement.unwrap()
                {
                    min_quantum_entanglement = Some(quantum_entanglement);
                }
            }
        }

        if let Some(min_quantum_entanglement) = min_quantum_entanglement {
            return min_quantum_entanglement;
        }
    }

    panic!("no valid configs found")
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
