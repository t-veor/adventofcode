#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

fn parse_input(input: String) -> String {
    input.trim().to_owned()
}

fn look_and_say(bytes: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();

    let mut i = 0;
    'outer: loop {
        let digit = bytes[i];

        for j in i + 1..bytes.len() {
            if bytes[j] != digit {
                let run_len = j - i;
                if run_len < 10 {
                    result.push(run_len as u8 + b'0');
                } else {
                    result.extend_from_slice(run_len.to_string().as_bytes());
                }
                result.push(digit);
                i = j;
                continue 'outer;
            }
        }

        // Reached the end of the string
        let run_len = bytes.len() - i;
        if run_len < 10 {
            result.push(run_len as u8 + b'0');
        } else {
            result.extend_from_slice(run_len.to_string().as_bytes());
        }
        result.push(digit);
        break;
    }

    result
}

fn star1(input: &str) -> usize {
    let mut x = input.as_bytes().to_vec();

    for _ in 0..40 {
        x = look_and_say(&x);
    }

    x.len()
}

fn star2(input: &str) -> usize {
    let mut x = input.as_bytes().to_vec();

    for _ in 0..50 {
        x = look_and_say(&x);
    }

    x.len()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
