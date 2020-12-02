#!/usr/bin/env run-cargo-script

#[derive(Debug)]
struct Password {
    range: (usize, usize),
    req: char,
    password: String,
}

// String::split_once is nightly only
fn split_once<'a>(s: &'a str, sep: &'a str) -> Option<(&'a str, &'a str)> {
    let mut it = s.splitn(2, sep);
    let first = it.next()?;
    let second = it.next()?;
    Some((first, second))
}

fn parse_password(s: &str) -> Password {
    let (head, password) = split_once(s, ": ").unwrap();
    let (range, req) = split_once(head, " ").unwrap();
    let (first, second) = split_once(range, "-").unwrap();

    Password {
        range: (first.parse().unwrap(), second.parse().unwrap()),
        req: req.chars().next().unwrap(),
        password: password.to_owned(),
    }
}

fn star1(input: &[Password]) -> usize {
    input
        .iter()
        .filter(|password| {
            let count = password
                .password
                .chars()
                .filter(|c| *c == password.req)
                .count();
            password.range.0 <= count && count <= password.range.1
        })
        .count()
}

fn star2(input: &[Password]) -> usize {
    input
        .iter()
        .filter(|password| {
            let first = password.password.chars().nth(password.range.0 - 1).unwrap();
            let second = password.password.chars().nth(password.range.1 - 1).unwrap();
            (first == password.req) ^ (second == password.req)
        })
        .count()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input: Vec<_> = std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .map(|i| parse_password(i))
        .collect();

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
