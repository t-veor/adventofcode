#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn balanced_quinary_to_i64(s: &str) -> Option<i64> {
    let mut x = 0;
    for c in s.as_bytes() {
        x *= 5;
        x += match c {
            b'2' => 2,
            b'1' => 1,
            b'0' => 0,
            b'-' => -1,
            b'=' => -2,
            _ => return None,
        };
    }
    Some(x)
}

fn i64_to_balanced_quinary(mut x: i64) -> String {
    let mut curr_position = 1;
    while curr_position * 2 < x.abs() {
        curr_position *= 5;
    }

    let mut result = String::new();
    while curr_position > 0 {
        // Feel like there should be a better equation for getting the nth digit
        // of a balanced quinary number, but I did come up with this one by
        // myself
        let digit = (x + curr_position * 5 / 2) / curr_position - 2;

        result.push(match digit {
            2 => '2',
            1 => '1',
            0 => '0',
            -1 => '-',
            -2 => '=',
            _ => unreachable!(),
        });

        x -= digit * curr_position;
        curr_position /= 5;
    }

    result
}

fn parse_input(input: String) -> String {
    input
}

fn star1(input: &str) -> String {
    let sum = input
        .lines()
        .map(|x| balanced_quinary_to_i64(x).unwrap())
        .sum();
    i64_to_balanced_quinary(sum)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
}
