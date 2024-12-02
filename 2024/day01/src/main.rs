use std::collections::HashMap;

use utils::read_input_file;

fn parse_input(input: &str) -> (Vec<i64>, Vec<i64>) {
    let mut left = Vec::new();
    let mut right = Vec::new();

    for line in input.trim().lines() {
        let mut elems = line.split_whitespace();
        let l = elems.next().unwrap();
        let r = elems.next().unwrap();

        left.push(l.parse().unwrap());
        right.push(r.parse().unwrap());
    }

    (left, right)
}

fn list_diff(left: &[i64], right: &[i64]) -> u64 {
    let mut left = left.to_vec();
    left.sort();

    let mut right = right.to_vec();
    right.sort();

    left.into_iter()
        .zip(right)
        .map(|(l, r)| l.abs_diff(r))
        .sum()
}

fn list_sim(left: &[i64], right: &[i64]) -> i64 {
    let mut right_counts = HashMap::new();
    for x in right {
        *right_counts.entry(x).or_default() += 1;
    }

    left.iter()
        .map(|&x| x * right_counts.get(&x).unwrap_or(&0))
        .sum()
}

fn star1(left: &[i64], right: &[i64]) -> u64 {
    list_diff(left, right)
}

fn star2(left: &[i64], right: &[i64]) -> i64 {
    list_sim(left, right)
}

fn main() {
    let input = read_input_file!();
    let (left, right) = parse_input(&input);

    println!("{}", star1(&left, &right));
    println!("{}", star2(&left, &right));
}
