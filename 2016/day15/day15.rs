#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

// Literally just the Chinese remainder theorem.

#[derive(Debug, Clone)]
struct Disc {
    num: i32,
    positions: i32,
    initial_position: i32,
}

fn egcd(a: i32, b: i32) -> (i32, i32, i32) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}

fn multiplicative_inverse(x: i32, base: i32) -> Option<i32> {
    let (g, x, _) = egcd(x, base);
    if g == 1 {
        Some(x.rem_euclid(base))
    } else {
        None
    }
}

fn chinese_remainder_theorem(residues: &[i32], bases: &[i32]) -> Option<i32> {
    if residues.len() != bases.len() {
        return None;
    }

    let product = bases.iter().product::<i32>();
    let mut sum = 0;

    for (&residue, &base) in residues.iter().zip(bases) {
        let p = product / base;
        sum += residue * multiplicative_inverse(p, base)? * p;
    }

    Some(sum.rem_euclid(product))
}

fn parse_input(input: &str) -> Vec<Disc> {
    fn extract(line: &str) -> Disc {
        let split: Vec<_> = line.split_ascii_whitespace().collect();
        let num = split[1].strip_prefix("#").unwrap().parse().unwrap();
        let positions = split[3].parse().unwrap();
        let initial_position = split[11].strip_suffix(".").unwrap().parse().unwrap();

        Disc {
            num,
            positions,
            initial_position,
        }
    }

    input.lines().map(extract).collect()
}

fn star1(input: &[Disc]) -> i32 {
    // Set up a system of congruences
    let mut residues = Vec::with_capacity(input.len());
    let mut bases = Vec::with_capacity(input.len());

    for disc in input {
        // For each disc, we want its position to be 0 when the ball reaches it.
        // If the ball is dropped at time=t, then the disc position when the
        // ball reaches it is
        // (disc.initial_position + t + disc.num) % disc.positions
        // So the congruence we set up is
        // (disc.initial_position + t + disc.num) `equiv` 0 (mod disc.positions)
        // which we can rearrange:
        // t `equiv` -disc.initial_position - disc.num (mod disc.positions)
        residues.push(-disc.initial_position - disc.num);
        bases.push(disc.positions);
    }

    chinese_remainder_theorem(&residues, &bases).unwrap()
}

fn star2(input: &[Disc]) -> i32 {
    let mut discs = input.to_vec();
    let max_num = discs.iter().map(|disc| disc.num).max().unwrap();
    discs.push(Disc {
        num: max_num + 1,
        positions: 11,
        initial_position: 0,
    });

    star1(&discs)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
