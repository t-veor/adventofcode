#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! md5 = "0.7"
//! ```

// This is like a repeat of day 4 from 2015. I don't really like this -- it just
// forces you to burn through millions of hashes and doesn't fit the theme of
// being able to write a fast algorithm for

fn parse_input(input: String) -> String {
    input.trim().to_owned()
}

fn star1(input: &str) -> String {
    let mut result = String::new();

    for i in 0.. {
        let data = format!("{input}{i}");
        let digest = md5::compute(&data).0;
        if digest[0] == 0 && digest[1] == 0 && digest[2] & 0xf0 == 0 {
            result.push_str(&format!("{:x}", digest[2]));

            if result.len() >= 8 {
                break;
            }
        }
    }

    result
}

fn star2(input: &str) -> String {
    let mut chars = [None; 8];
    for i in 0.. {
        let data = format!("{input}{i}");
        let digest = md5::compute(&data).0;
        if digest[0] == 0 && digest[1] == 0 && digest[2] & 0xf0 == 0 {
            let position = digest[2];
            let char = (digest[3] & 0xf0) >> 4;

            if position < 8 {
                chars[position as usize].get_or_insert(char);
                if chars.iter().all(|i| i.is_some()) {
                    break;
                }
            }
        }
    }

    chars
        .into_iter()
        .map(|char| format!("{:x}", char.unwrap()).chars().next().unwrap())
        .collect()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
