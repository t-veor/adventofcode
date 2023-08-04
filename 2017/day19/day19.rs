#!/usr/bin/env rust-script
//! ```cargo
//! [package]
//! edition = "2021"
//! ```

use std::ops::Index;

struct Map {
    map: Vec<u8>,
    width: usize,
    start: (i32, i32),
}

impl Index<(i32, i32)> for Map {
    type Output = u8;

    fn index(&self, (x, y): (i32, i32)) -> &Self::Output {
        let index = y * self.width as i32 + x;
        index
            .try_into()
            .ok()
            .and_then(|index: usize| self.map.get(index))
            .unwrap_or(&b' ')
    }
}

fn parse_input(input: &str) -> Map {
    let width = input
        .lines()
        .map(|l| l.trim_end_matches(|c: char| c.is_ascii_control()).len())
        .max()
        .unwrap();
    let height = input.lines().count();

    let mut map = vec![b' '; width * height];
    let mut start = None;

    for (y, line) in input.lines().enumerate() {
        for (x, &c) in line
            .trim_end_matches(|c: char| c.is_ascii_control())
            .as_bytes()
            .iter()
            .enumerate()
        {
            if y == 0 && c == b'|' {
                start = Some((x as i32, y as i32));
            }
            map[x + y * width] = c;
        }
    }

    Map {
        map,
        width,
        start: start.unwrap(),
    }
}

fn walk(map: &Map) -> (String, u32) {
    let mut encountered = String::new();
    let mut pos = map.start;
    let mut dir = (0, 1);

    let mut steps = 0;

    loop {
        match map[pos] {
            b'+' => {
                dir = if dir.0 == 0 {
                    if map[(pos.0 + 1, pos.1)] != b' ' {
                        (1, 0)
                    } else {
                        (-1, 0)
                    }
                } else {
                    if map[(pos.0, pos.1 + 1)] != b' ' {
                        (0, 1)
                    } else {
                        (0, -1)
                    }
                }
            }
            c if c.is_ascii_uppercase() => encountered.push(c as char),
            b' ' => break,
            _ => (),
        }

        pos.0 += dir.0;
        pos.1 += dir.1;

        steps += 1;
    }

    (encountered, steps)
}

fn star1(input: &Map) -> String {
    walk(input).0
}

fn star2(input: &Map) -> u32 {
    walk(input).1
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let filename = args.get(1).map(|s| &s[..]).unwrap_or("input.txt");
    let input = std::fs::read_to_string(filename).unwrap();

    let input = parse_input(&input);

    println!("{}", star1(&input));
    println!("{}", star2(&input));
}
