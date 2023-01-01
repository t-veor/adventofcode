#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::{collections::HashMap, str::FromStr};

#[derive(Debug, Clone)]
struct RoomName {
    name: String,
    sector_id: i32,
    checksum: String,
}

impl FromStr for RoomName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (body, checksum) = s.split_once('[').ok_or(())?;
        let checksum = checksum.strip_suffix(']').ok_or(())?;
        let sector_id_idx = body.rfind('-').ok_or(())?;
        let (name, sector_id) = (&body[..sector_id_idx], &body[sector_id_idx + 1..]);
        Ok(Self {
            name: name.to_owned(),
            sector_id: sector_id.parse().map_err(|_| ())?,
            checksum: checksum.to_owned(),
        })
    }
}

impl RoomName {
    fn is_real(&self) -> bool {
        let mut freqs = HashMap::new();
        for c in self.name.chars() {
            if c != '-' {
                *freqs.entry(c).or_insert(0) += 1;
            }
        }
        let mut sorted_freqs: Vec<_> = freqs.into_iter().map(|(c, freq)| (-freq, c)).collect();
        sorted_freqs.sort_unstable();
        let required_checksum: String = sorted_freqs.into_iter().take(5).map(|(_, c)| c).collect();
        self.checksum == required_checksum
    }

    fn decrypt(&self) -> String {
        let mut result = String::with_capacity(self.name.len());
        let a = 'a' as u32;
        for c in self.name.chars() {
            result.push(if c == '-' {
                ' '
            } else {
                char::from_u32(((c as u32 - a) + self.sector_id as u32) % 26 + a).unwrap()
            });
        }
        result
    }
}

fn parse_input(input: String) -> Vec<RoomName> {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn star1(input: &[RoomName]) -> i32 {
    input
        .iter()
        .filter(|name| name.is_real())
        .map(|name| name.sector_id)
        .sum()
}

fn star2(input: &[RoomName]) -> i32 {
    for i in input {
        if i.is_real() && i.decrypt() == "northpole object storage" {
            return i.sector_id;
        }
    }

    panic!("`northpole object storage' not found")
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
