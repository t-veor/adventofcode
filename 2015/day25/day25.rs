#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

const COEFF: u64 = 252533;
const BASE: u64 = 33554393;
const INITIAL: u64 = 20151125;

fn parse_input(input: String) -> (u64, u64) {
    let split: Vec<_> = input.trim().split_ascii_whitespace().collect();
    let row = split[split.len() - 3].strip_suffix(',').unwrap();
    let column = split[split.len() - 1].strip_suffix('.').unwrap();

    (row.parse().unwrap(), column.parse().unwrap())
}

fn get_position_from_coordinate(row: u64, column: u64) -> u64 {
    // Work out which diagonal the coordinate is on. E.g. (1, 1) is on the first
    // diagonal, (2, 1) and (1, 2) are on the second diagonal, (3, 1), (2, 2),
    // (1, 3) are on the third diagonal, etc.
    let diagonal = row + column - 1;
    // Work out where along this diagonal the coordinate is. This is actually
    // just reading off the column - e.g. (3, 1) is the 1st number on the 3rd
    // diagonal, (2, 2) is the 2nd number on the 3rd diagonal, (1, 3) is the 3rd
    // number on the 3rd diagonal, etc.
    let nth_on_diagonal = column;
    // The count of numbers before the current diagonal is just 1 + 2 + 3 + ...
    // + (diagonal - 1), or the (diagonal - 1)-th triangle number.
    let numbers_before_curr_diagonal = (diagonal - 1) * diagonal / 2;
    // Finally, the final position is simply the count of numbers before the
    // current diagonal, plus the position on the current diagonal.
    numbers_before_curr_diagonal + nth_on_diagonal
}

fn pow_modulo(mut x: u64, mut power: u64, base: u64) -> u64 {
    // Simple exponentiation by squaring, to calculate x^n in O(log n) time.
    if power == 0 {
        return 1;
    }
    let mut y = 1;
    while power > 1 {
        if power % 2 == 1 {
            y = (x * y) % base;
        }
        x = (x * x) % base;
        power /= 2;
    }
    (x * y) % base
}

fn code_at(row: u64, column: u64) -> u64 {
    let n = get_position_from_coordinate(row, column);
    let m = pow_modulo(COEFF, n - 1, BASE);
    (m * INITIAL) % BASE
}

fn star1(input: &(u64, u64)) -> u64 {
    code_at(input.0, input.1)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
}
