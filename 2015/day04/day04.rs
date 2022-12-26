#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! md5 = "0.7"
//! ```

fn parse_input(input: String) -> String {
    input.trim().to_owned()
}

fn star1(input: &str) -> i32 {
    for i in 1.. {
        let data = format!("{input}{i}");
        let digest = md5::compute(&data).0;
        if digest[0] == 0 && digest[1] == 0 && digest[2] & 0xf0 == 0 {
            return i;
        }
    }

    unreachable!()
}

fn star2(input: &str) -> i32 {
    for i in 1.. {
        let data = format!("{input}{i}");
        let digest = md5::compute(&data).0;
        if digest[0] == 0 && digest[1] == 0 && digest[2] == 0 {
            return i;
        }
    }

    unreachable!()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
