#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Packet {
    Int(i32),
    List(Vec<Packet>),
}

impl Packet {
    // So at this point, I would probably actually pull in some sort of parser
    // combinator library like nom instead of handrolling my awful parser, but I
    // don't want to add a dependency
    fn try_parse(s: &[u8]) -> Option<(Self, &[u8])> {
        match *s.get(0)? {
            b'[' => {
                let mut list = Vec::new();
                let mut s = s.get(1..)?;
                while *s.get(0)? != b']' {
                    let result = Self::try_parse(s)?;
                    list.push(result.0);
                    s = result.1;

                    if *s.get(0)? == b',' {
                        s = &s[1..];
                    }
                }
                let remainder = &s[1..];
                Some((Packet::List(list), remainder))
            }
            _ => {
                let mut num_end = s.len();
                for (i, c) in s.iter().enumerate() {
                    if !c.is_ascii_digit() {
                        num_end = i;
                        break;
                    }
                }
                let num = std::str::from_utf8(&s[..num_end]).ok()?.parse().ok()?;
                let remainder = &s[num_end..];
                Some((Packet::Int(num), remainder))
            }
        }
    }
}

impl FromStr for Packet {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (packet, remainder) = Self::try_parse(s.as_bytes()).ok_or(())?;
        if remainder.is_empty() {
            Ok(packet)
        } else {
            Err(())
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Packet::Int(a), Packet::Int(b)) => a.cmp(b),
            (Packet::List(a), Packet::List(b)) => a.cmp(b),
            (Packet::Int(a), Packet::List(b)) => vec![Packet::Int(*a)].cmp(b),
            (Packet::List(a), Packet::Int(b)) => a.cmp(&vec![Packet::Int(*b)]),
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn parse_input(input: String) -> Vec<Packet> {
    input
        .lines()
        .filter_map(|line| {
            if !line.is_empty() {
                Some(line.parse().unwrap())
            } else {
                None
            }
        })
        .collect()
}

fn star1(input: &[Packet]) -> usize {
    input
        .chunks_exact(2)
        .enumerate()
        .map(|(i, chunk)| if chunk[0] < chunk[1] { i + 1 } else { 0 })
        .sum()
}

fn star2(input: &[Packet]) -> usize {
    let decoder_2: Packet = "[[2]]".parse().unwrap();
    let decoder_6: Packet = "[[6]]".parse().unwrap();

    let mut all_packets = Vec::with_capacity(input.len() + 2);
    all_packets.extend_from_slice(input);
    all_packets.push(decoder_2.clone());
    all_packets.push(decoder_6.clone());

    all_packets.sort();

    let decoder_2_pos = all_packets.iter().position(|p| p == &decoder_2).unwrap();
    let decoder_6_pos = all_packets.iter().position(|p| p == &decoder_6).unwrap();

    (decoder_2_pos + 1) * (decoder_6_pos + 1)
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
